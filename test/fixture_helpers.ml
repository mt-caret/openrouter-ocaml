open! Core
open! Async
open Openrouter_api

(* Response records carry [@@jsonaf.allow_extra_fields.log], so parsing a fixture
   whose body has a field we don't model emits an [Error]-level log line naming
   it. Pin the global log's time source to the epoch so the lines get a stable
   timestamp, and capture them into a buffer that [flush_log] drains into the
   expect snapshot after the parsed value is printed.

   [flush_log] collapses identical lines: a /models response repeats the same
   "extra fields" warning once per entry (300+ times), which says nothing more
   than the single line does. Distinct warnings are kept, sorted for stability. *)
let captured_log_lines : string Queue.t = Queue.create ()

let () =
  Synchronous_time_source.create ~now:Time_ns.epoch ()
  |> Synchronous_time_source.read_only
  |> Log.Global.set_time_source;
  Log.Global.set_output
    [ Log.Output.create
        ~flush:(fun () -> return ())
        (fun msgs ->
           Queue.iter msgs ~f:(fun msg ->
             Queue.enqueue captured_log_lines (Log.Message.to_write_only_text msg));
           return ())
    ]
;;

let flush_log () =
  let%map () = Log.Global.flushed () in
  Queue.to_list captured_log_lines
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline;
  Queue.clear captured_log_lines
;;

(* Each fixture lives in [responses/<name>.{json,ndjson}] and exercises a real
   captured response body. The expected sexp is snapshot-tested so that any
   future refactor that breaks parsing fails loudly here.

   Capture flags on the example binary write the raw response body so adding
   a fixture is one shell command; pick the right helper for the shape:

   - [parse_response_fixture]   — chat /chat/completions response (one JSON file).
   - [parse_stream_fixture]     — chat streaming chunks (NDJSON; one chunk per line).
   - [parse_embeddings_fixture] — /embeddings response.
   - [parse_generation_fixture] — /generation response.
   - [parse_api_error_fixture]  — error envelope from any endpoint.

   Don't add a fixture that just differs by model id — these tests are about
   JSON shapes, not provider coverage. Pick combinations that produce a
   structurally distinct response. *)

let parse_fixture ~of_jsonaf ~sexp_of name ~ext =
  let%bind body = Reader.file_contents [%string "responses/%{name}.%{ext}"] in
  body |> Jsonaf.parse |> Or_error.ok_exn |> of_jsonaf |> sexp_of |> print_s;
  flush_log ()
;;

let parse_response_fixture =
  parse_fixture
    ~of_jsonaf:Completions.For_testing.response_of_jsonaf
    ~sexp_of:[%sexp_of: Completions.Response.t]
    ~ext:"json"
;;

let stream_fixture_chunks name =
  let%bind body = Reader.file_contents [%string "responses/%{name}.ndjson"] in
  body
  |> String.split_lines
  |> List.filter_map ~f:(fun line ->
    match String.strip line with
    | "" -> None
    | line ->
      line
      |> Jsonaf.parse
      |> Or_error.ok_exn
      |> Completions.For_testing.stream_chunk_of_jsonaf
      |> Some)
  |> Deferred.return
;;

let parse_stream_fixture name =
  let%bind chunks = stream_fixture_chunks name in
  print_s [%sexp (chunks : Completions.Stream_chunk.t list)];
  flush_log ()
;;

module Audio_stream_summary = struct
  type t =
    { chunks : int
    ; audio_chunks : int
    ; first_audio_id : string option
    ; transcripts : string list
    ; data_chunks : int
    ; expires_at_chunks : int
    ; audio_tokens : int option
    }
  [@@deriving sexp]
end

let audio_stream_summary chunks =
  let audio_chunks =
    List.concat_map chunks ~f:(fun (chunk : Completions.Stream_chunk.t) ->
      List.filter_map chunk.choices ~f:(fun choice -> choice.delta.audio))
  in
  let audio_tokens =
    chunks
    |> List.find_map ~f:(fun (chunk : Completions.Stream_chunk.t) ->
      Option.bind chunk.usage ~f:(fun usage ->
        Option.bind usage.completion_tokens_details ~f:(fun details ->
          details.audio_tokens)))
  in
  { Audio_stream_summary.chunks = List.length chunks
  ; audio_chunks = List.length audio_chunks
  ; first_audio_id = List.find_map audio_chunks ~f:(fun audio -> audio.id)
  ; transcripts = List.filter_map audio_chunks ~f:(fun audio -> audio.transcript)
  ; data_chunks = List.count audio_chunks ~f:(fun audio -> Option.is_some audio.data)
  ; expires_at_chunks =
      List.count audio_chunks ~f:(fun audio -> Option.is_some audio.expires_at)
  ; audio_tokens
  }
;;

let parse_embeddings_fixture =
  parse_fixture
    ~of_jsonaf:Embeddings.For_testing.response_of_jsonaf
    ~sexp_of:[%sexp_of: Embeddings.Response.t]
    ~ext:"json"
;;

let parse_generation_fixture =
  parse_fixture
    ~of_jsonaf:Generation.For_testing.stats_of_jsonaf
    ~sexp_of:[%sexp_of: Generation.Stats.t]
    ~ext:"json"
;;

let parse_api_error_fixture =
  parse_fixture
    ~of_jsonaf:[%of_jsonaf: Api_error.t]
    ~sexp_of:[%sexp_of: Api_error.t]
    ~ext:"json"
;;
