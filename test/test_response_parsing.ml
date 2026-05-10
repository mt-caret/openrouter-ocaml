open! Core
open! Async
open Openrouter_api

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
  Deferred.unit
;;

let parse_response_fixture =
  parse_fixture
    ~of_jsonaf:Completions.For_testing.response_of_jsonaf
    ~sexp_of:[%sexp_of: Completions.Response.t]
    ~ext:"json"
;;

let parse_stream_fixture name =
  let%bind body = Reader.file_contents [%string "responses/%{name}.ndjson"] in
  let chunks =
    String.split_lines body
    |> List.filter_map ~f:(fun line ->
      match String.strip line with
      | "" -> None
      | line ->
        line
        |> Jsonaf.parse
        |> Or_error.ok_exn
        |> Completions.For_testing.stream_chunk_of_jsonaf
        |> Some)
  in
  print_s [%sexp (chunks : Completions.Stream_chunk.t list)];
  Deferred.unit
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

(* Anthropic via Amazon Bedrock: the response sets [content] to [null] when
   the assistant emits tool_calls instead of text, sets [system_fingerprint]
   explicitly to [null] (rather than omitting it), and populates [tool_calls]
   on the message. The [system_fingerprint: null] case is a regression guard:
   it used to crash the parser because the field was annotated with
   [[@jsonaf.option]], which doesn't tolerate explicit nulls. *)
let%expect_test "bedrock_null_system_fingerprint" =
  let%bind () = parse_response_fixture "bedrock_null_system_fingerprint" in
  [%expect
    {|
    ((id gen-1778392451-h13oB9BVTj3ty6FES1xt) (provider "Amazon Bedrock")
     (model anthropic/claude-4.6-sonnet-20260217) (object_ chat.completion)
     (created 1778392451)
     (choices
      (((logprobs ()) (finish_reason tool_calls) (native_finish_reason tool_use)
        (index 0)
        (message
         ((role assistant) (content ()) (refusal ()) (reasoning ())
          (reasoning_details ()) (images ()) (annotations ())
          (tool_calls
           (((id toolu_vrtx_01TRxp7ru5sZbR2D5MzZGpgv) (type_ function)
             (function_
              ((name classify_post) (arguments "{\"ocaml_related\": true}"))))))
          (tool_call_id ()))))))
     (system_fingerprint ()) (service_tier ())
     (usage
      ((prompt_tokens 18961) (completion_tokens 37) (total_tokens 18998)
       (cost (0.057438)) (is_byok ()) (prompt_tokens_details ())
       (cost_details ()) (completion_tokens_details ()) (server_tool_use ()))))
    |}];
  Deferred.unit
;;

(* OpenAI direct, with [logprobs=true] and [top_logprobs=N] — exercises the
   full [Logprobs.t] / [Logprobs.Token.t] / [Logprobs.Top_logprob.t] shape. *)
let%expect_test "openai_logprobs" =
  let%bind () = parse_response_fixture "openai_logprobs" in
  [%expect
    {|
    ((id gen-1778402842-D7D4RjsqmSgRPZyPCL6J) (provider Azure)
     (model openai/gpt-4o-mini) (object_ chat.completion) (created 1778402842)
     (choices
      (((logprobs
         (((content
            ((((token pong) (logprob -3.8577192754019052E-05)
               (bytes ((112 111 110 103)))
               (top_logprobs
                (((token pong) (logprob -3.8577192754019052E-05)
                  (bytes ((112 111 110 103))))
                 ((token P) (logprob -10.250038146972656) (bytes ((80))))
                 ((token " pong") (logprob -13.000038146972656)
                  (bytes ((32 112 111 110 103))))))))))
           (refusal ()))))
        (finish_reason stop) (native_finish_reason stop) (index 0)
        (message
         ((role assistant) (content (pong)) (refusal ()) (reasoning ())
          (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())
          (tool_call_id ()))))))
     (system_fingerprint (fp_eb37e061ec)) (service_tier ())
     (usage
      ((prompt_tokens 14) (completion_tokens 2) (total_tokens 16)
       (cost (3.3E-06)) (is_byok (false))
       (prompt_tokens_details
        (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
          (video_tokens (0)))))
       (cost_details
        (((upstream_inference_cost (3.3E-06))
          (upstream_inference_prompt_cost (2.1E-06))
          (upstream_inference_completions_cost (1.2E-06)))))
       (completion_tokens_details
        (((reasoning_tokens (0)) (image_tokens (0)) (audio_tokens (0)))))
       (server_tool_use ()))))
    |}];
  Deferred.unit
;;

(* OpenAI's o-series style reasoning: [reasoning_details] entries carry a
   [data] field (encrypted reasoning) instead of [text], the [format] is
   [openai-responses-v1], and [service_tier] is populated. *)
let%expect_test "openai_gpt5_mini_reasoning" =
  let%bind () = parse_response_fixture "openai_gpt5_mini_reasoning" in
  [%expect
    {|
    ((id gen-1778402845-xrFo6pNnn7qF6DnmJRdp) (provider OpenAI)
     (model openai/gpt-5-mini-2025-08-07) (object_ chat.completion)
     (created 1778402845)
     (choices
      (((logprobs ()) (finish_reason stop) (native_finish_reason completed)
        (index 0)
        (message
         ((role assistant) (content ("1 + 1 = 2")) (refusal ()) (reasoning ())
          (reasoning_details
           (((format (openai-responses-v1)) (index (0))
             (type_ (reasoning.encrypted)) (text ()) (signature ())
             (data
              (gAAAAABqAEYeCqRFfityy7cfJA6FhAHCpXvg5esml-wiuX4bzdtF5yAirFR1XNxPxDw0rsDM-oedBgca8bbpeAIPd_U6UBnHQDxeGcB4mEwC4TNE57IxmlaU0LGhALC-q2j84UtGHVp4yro7qZR9Fwgb_nlvk0FOlTIZdVL1BRHXywfqy6990LuTfarKfzti7TWRyTavkbhsyKFnX9WNpLTANYdW28Yn0U2g4KFk2Xa36SJSGvlLOxL-Y1kQnzr9DZolNkOwZYEXBwggO68Iy36ICuJ0waVbYQNJGcOntcIBtiTCRil6o8ED0uQMxT30cF1af8FZgxHDAjK-4Jr1Th6UhQXbSvzrR1im9vUIp3L8AewfOKayt47NaRNYEddeoC06cYZivuaht-xZACXukFFzW1tvDA2Q-vVrXzojepiNXQMoBkWaqGmBhzqfXDnZ7U92gXq0FhHRni38EZHszLZvcChJiolRdcYGRKN5kyHPUJoBvzVZSPCKzGdmusqsqfcPK6rY4PkbtcG78Ga7ZTKl-MhmMZdpHPGBs7amxHoqPV52j6ffPre-sRAHw6TPdZvy-GkG_S540zUajqIfT9ile4WDTayP63CHsjyLGSMfXLeAKG7IeuFhTLiel-3igZLw2uA6N1BSXwvUhQLFUEzzUvWI15Q66UxJzuVZfCbpADT4PelLwSeW9Vl0w9M2ROf50nqTzRXRNfT1gJAs1UevqWgVMhRDAEK4wvV_9YXApFEMJpxhbwMst8Y9OqpZMH-DtRy0HIfFj0Mbie5gzbFMMvkgGQgyJ6pPhAEVMv0bQkrEDvXj0czx965UMiox7SSE_5aqPM7wSYlcvP9as7G-1o18SO3-zBDsvddZti1pNnk7h9BjmZVaA8iN-DAioCS1W12CTSy7-ZZwTJkq38muzgMMaQn_jhAbhJa4OG7n8gpv8MldSg8jPNIvwtZK-PELWT_h59mwMT3vgPttD_b-5yQB86DF_w==)))))
          (images ()) (annotations ()) (tool_calls ()) (tool_call_id ()))))))
     (system_fingerprint ()) (service_tier (default))
     (usage
      ((prompt_tokens 10) (completion_tokens 25) (total_tokens 35)
       (cost (5.25E-05)) (is_byok (false))
       (prompt_tokens_details
        (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
          (video_tokens (0)))))
       (cost_details
        (((upstream_inference_cost (5.25E-05))
          (upstream_inference_prompt_cost (2.5E-06))
          (upstream_inference_completions_cost (5E-05)))))
       (completion_tokens_details
        (((reasoning_tokens (0)) (image_tokens (0)) (audio_tokens (0)))))
       (server_tool_use ()))))
    |}];
  Deferred.unit
;;

(* Anthropic-style reasoning: [reasoning_details] entries carry both [text]
   and [signature], [format] is [anthropic-claude-v1], and the top-level
   [reasoning] string mirrors the detail text. *)
let%expect_test "anthropic_haiku_reasoning" =
  let%bind () = parse_response_fixture "anthropic_haiku_reasoning" in
  [%expect
    {|
    ((id gen-1778402848-djGRviCSIfYMw7vLL8Dt) (provider "Amazon Bedrock")
     (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion)
     (created 1778402848)
     (choices
      (((logprobs ()) (finish_reason stop) (native_finish_reason end_turn)
        (index 0)
        (message
         ((role assistant)
          (content
           ( "# 1 + 1\
            \n\
            \n**Steps:**\
            \n1. Start with 1\
            \n2. Add 1 more\
            \n3. Count: 1, 2\
            \n\
            \n**Answer: 2**"))
          (refusal ())
          (reasoning
           ( "This is a very simple arithmetic problem. The user is asking me to calculate 1+1 and show the steps briefly.\
            \n\
            \n1 + 1 = 2\
            \n\
            \nThe steps are straightforward:\
            \n- Start with 1\
            \n- Add 1 to it\
            \n- Get 2\
            \n\
            \nLet me present this concisely."))
          (reasoning_details
           (((format (anthropic-claude-v1)) (index (0)) (type_ (reasoning.text))
             (text
              ( "This is a very simple arithmetic problem. The user is asking me to calculate 1+1 and show the steps briefly.\
               \n\
               \n1 + 1 = 2\
               \n\
               \nThe steps are straightforward:\
               \n- Start with 1\
               \n- Add 1 to it\
               \n- Get 2\
               \n\
               \nLet me present this concisely."))
             (signature
              (EoIDCkgIDRABGAIqQNJ68Ljc66/k6FlS0Ams/FZeke4eeQ2Tx28quZanENJXCYcCuYyNP7E0VPjpm8b5fOhOICt+dmYouGFKCZ2ZjPQSDFjoVMgjDW+HIT5m1RoMbZl4Zx/hvww6bwERIjDnqLUqGgZDhDCQf15MoI50GNnstQeyNNtI7hhEU8J6SYmEN5P6Crt8b77bnwFfQj4q5wH5i7oGbbexEO2NNtd44n+adtujt6t2nLAeTZuNknj/Hv/uIGd690u+wrXsz1XOT02sdH/1n6XwvQWIUQbbmpOwOcX33vELsI/+CiAHVg3o1n76z8DqSWa3o+hGBTNnU/7yqzJQJpZgvXq/zwgE4LSxU+qNSJJgHLWogvgPO5z7uLpcE0NLYtMYRS7z2RBdn9CZwfqnZ+nOYbKFu5bJNrnioLsA9gavFmaNDFJ2xz/pT4Nev9RVYn1JZJSk8rs8Di5t5rCQJpcG66qGPGdgOIiEOB5vSCrwwmEGZ22u3CXwhJBYhXYJ8fAYAQ==))
             (data ()))))
          (images ()) (annotations ()) (tool_calls ()) (tool_call_id ()))))))
     (system_fingerprint ()) (service_tier ())
     (usage
      ((prompt_tokens 44) (completion_tokens 125) (total_tokens 169)
       (cost (0.000669)) (is_byok (false))
       (prompt_tokens_details
        (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
          (video_tokens (0)))))
       (cost_details
        (((upstream_inference_cost (0.000669))
          (upstream_inference_prompt_cost (4.4E-05))
          (upstream_inference_completions_cost (0.000625)))))
       (completion_tokens_details
        (((reasoning_tokens (62)) (image_tokens (0)) (audio_tokens (0)))))
       (server_tool_use ()))))
    |}];
  Deferred.unit
;;

(* DeepSeek R1: reasoning is exposed as a plain [reasoning] string plus a
   single [reasoning_details] entry with [text] only — no [signature] and no
   [data]. *)
let%expect_test "deepseek_r1" =
  let%bind () = parse_response_fixture "deepseek_r1" in
  [%expect
    {|
    ((id gen-1778402852-xCuZReg39HG9m6OHoD23) (provider Azure)
     (model deepseek/deepseek-r1) (object_ chat.completion) (created 1778402852)
     (choices
      (((logprobs ()) (finish_reason stop) (native_finish_reason stop) (index 0)
        (message
         ((role assistant)
          (content
           ("The result of 2 + 2 is **4**. This is a fundamental arithmetic operation where two quantities are combined to produce their total sum."))
          (refusal ())
          (reasoning
           ( "Okay, so the user is asking \"what is 2+2\". Hmm, that seems pretty straightforward. Let me think. I know that addition is one of the basic arithmetic operations. When you add two numbers, you're essentially combining their values. So, if I have 2 apples and someone gives me 2 more apples, how many apples do I have in total? That's right, 4 apples.\
            \n\
            \nWait, but maybe I should double-check to make sure I'm not missing anything here. Sometimes even simple questions can have nuances. Let me visualize it. If I take the number 2 and then add another 2 to it, counting up: 2, then 3, 4. So that's two steps. Yep, that gets me to 4. \
            \n\
            \nIs there a different way to approach this? Maybe using objects. If I have two groups of two items each, combining them would result in four items total. For example, two pairs of shoes make four shoes. That still adds up to 4. \
            \n\
            \nI could also use a number line. Starting at 0, move two units to the right, landing on 2. Then move another two units to the right. That would be at 4. Definitely, the result is 4. \
            \n\
            \nIs there any context where 2+2 might not equal 4? Well, in some non-standard mathematical systems or under certain operations, maybe, but in basic arithmetic, which is what the user is likely asking about, 2+2 is always 4. \
            \n\
            \nLet me think if there's a trick here. The question is very simple, but sometimes people use such questions to test if someone is paying attention. But no, in this case, it's just a straightforward addition. \
            \n\
            \nAnother way to confirm: using fingers. Hold up two fingers on one hand and two on the other. Count all the fingers: 1, 2, 3, 4. Yep, that's four. \
            \n\
            \nSo, all methods point to the answer being 4. I don't see any reason to doubt that. The user might be testing me, but I'm confident in this answer. Just to be thorough, maybe check with a calculator. If I input 2 + 2, the calculator shows 4. \
            \n\
            \nAlright, I think that's solid. The answer is 4.\
            \n"))
          (reasoning_details
           (((format (unknown)) (index (0)) (type_ (reasoning.text))
             (text
              ( "Okay, so the user is asking \"what is 2+2\". Hmm, that seems pretty straightforward. Let me think. I know that addition is one of the basic arithmetic operations. When you add two numbers, you're essentially combining their values. So, if I have 2 apples and someone gives me 2 more apples, how many apples do I have in total? That's right, 4 apples.\
               \n\
               \nWait, but maybe I should double-check to make sure I'm not missing anything here. Sometimes even simple questions can have nuances. Let me visualize it. If I take the number 2 and then add another 2 to it, counting up: 2, then 3, 4. So that's two steps. Yep, that gets me to 4. \
               \n\
               \nIs there a different way to approach this? Maybe using objects. If I have two groups of two items each, combining them would result in four items total. For example, two pairs of shoes make four shoes. That still adds up to 4. \
               \n\
               \nI could also use a number line. Starting at 0, move two units to the right, landing on 2. Then move another two units to the right. That would be at 4. Definitely, the result is 4. \
               \n\
               \nIs there any context where 2+2 might not equal 4? Well, in some non-standard mathematical systems or under certain operations, maybe, but in basic arithmetic, which is what the user is likely asking about, 2+2 is always 4. \
               \n\
               \nLet me think if there's a trick here. The question is very simple, but sometimes people use such questions to test if someone is paying attention. But no, in this case, it's just a straightforward addition. \
               \n\
               \nAnother way to confirm: using fingers. Hold up two fingers on one hand and two on the other. Count all the fingers: 1, 2, 3, 4. Yep, that's four. \
               \n\
               \nSo, all methods point to the answer being 4. I don't see any reason to doubt that. The user might be testing me, but I'm confident in this answer. Just to be thorough, maybe check with a calculator. If I input 2 + 2, the calculator shows 4. \
               \n\
               \nAlright, I think that's solid. The answer is 4.\
               \n"))
             (signature ()) (data ()))))
          (images ()) (annotations ()) (tool_calls ()) (tool_call_id ()))))))
     (system_fingerprint ()) (service_tier ())
     (usage
      ((prompt_tokens 11) (completion_tokens 524) (total_tokens 535)
       (cost (0.003128895)) (is_byok (false))
       (prompt_tokens_details
        (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
          (video_tokens (0)))))
       (cost_details
        (((upstream_inference_cost (0.003128895))
          (upstream_inference_prompt_cost (1.6335E-05))
          (upstream_inference_completions_cost (0.00311256)))))
       (completion_tokens_details
        (((reasoning_tokens (492)) (image_tokens (0)) (audio_tokens (0)))))
       (server_tool_use ()))))
    |}];
  Deferred.unit
;;

(* Web search plugin: response carries [annotations] with [url_citation]
   entries. Note that the captured body has leading SSE-style keepalive
   whitespace before the JSON object — the parser must tolerate that. *)
let%expect_test "web_search" =
  let%bind () = parse_response_fixture "web_search" in
  [%expect
    {|
    ((id gen-1778402879-FcYUjHH155AstOdYflSR) (provider Azure)
     (model openai/gpt-4o-mini) (object_ chat.completion) (created 1778402879)
     (choices
      (((logprobs ()) (finish_reason stop) (native_finish_reason stop) (index 0)
        (message
         ((role assistant) (content (Paris.)) (refusal ()) (reasoning ())
          (reasoning_details ()) (images ())
          (annotations
           ((Object
             ((type (String url_citation))
              (url_citation
               (Object
                ((url
                  (String
                   https://dictionary.cambridge.org/us/dictionary/english/paris))
                 (title
                  (String
                   "Paris | definition in the Cambridge English Dictionary"))
                 (start_index (Number 0)) (end_index (Number 0))
                 (content
                  (String
                    "Paris | definition in the Cambridge English Dictionary \
                   \n\
                   \n# Meaning of Paris in English\
                   \n\
                   \nParis\
                   \n\
                   \nnoun\
                   \n\
                   \nus\
                   \n\
                   \nYour browser doesn't support HTML5 audio\
                   \n\
                   \n/\203\136per.\201\170s/ uk\
                   \n\
                   \nYour browser doesn't support HTML5 audio\
                   \n\
                   \n/\203\136p\195\166r.\201\170s/\
                   \n\
                   \nAdd to word list Add to word list\
                   \n\
                   \nthe capital city of France, situated in the north central part of the country\
                   \n\
                   \nTowns & regions: city names & their inhabitants\
                   \n\
                   \n- Aberdeen\
                   \n- Aberdonian\
                   \n- Abidjan\
                   \n- Abu Dhabi\
                   \n- Abuja\
                   \n- Columbia\
                   \n- Columbus\
                   \n- Conakry\
                   \n- Concepci\195\179n\
                   \n- Constantinople\
                   \n- Little Rock\
                   \n- Liverpool\
                   \n- Lobamba\
                   \n- London\
                   \n- Londonderry\
                   \n- Samarkand\
                   \n- San Antonio\
                   \n- San Diego\
                   \n- San Francisco\
                   \n- San Jose\
                   \n\
                   \nSee more results \194\187\
                   \n\
                   \n(Definition of Paris from the Cambridge Advanced Learner's Dictionary & Thesaurus\194\169 Cambridge University Press)\
                   \n\
                   \nWhat is the pronunciation of Paris?\
                   \n\
                   \n \
                   \n\
                   \n## Translations of Paris\
                   \n\
                   \n \
                   \n\
                   \nin Chinese (Traditional)\
                   \n\
                   \n\229\183\180\233\187\142\239\188\136\230\179\149\229\156\139\233\166\150\233\131\189\239\188\137\226\128\166\
                   \n\
                   \nSee more\
                   \n\
                   \nin Chinese (Simplified)\
                   \n\
                   \n\229\183\180\233\187\142\239\188\136\230\179\149\229\155\189\233\166\150\233\131\189\239\188\137\226\128\166\
                   \n\
                   \nSee more\
                   \n\
                   \nin Spanish\
                   \n\
                   \nPar\195\173s\226\128\166\
                   \n\
                   \nSee more\
                   \n\
                   \nin Portuguese\
                   \n\
                   \nParis\226\128\166\
                   \n\
                   \nSee more\
                   \n\
                   \n \
                   \n\
                   \nNeed a translator?\
                   \n\
                   \nGet a quick, free translation!\
                   \n\
                   \nTranslator tool\
                   \n\
                   \n## Browse\
                   \n\
                   \nparietal lobe\
                   \n\
                   \nparing\
                   \n\
                   \nparing knife\
                   \n\
                   \nparipinnate BETA\
                   \n\
                   \nParis\
                   \n\
                   \nParis Club\
                   \n\
                   \nparish\
                   \n\
                   \nparish church BETA\
                   \n\
                   \nparish clerk\
                   \n\
                   \nWord of the Day\
                   \n\
                   \ngreek\
                   \n\
                   \nUK\
                   \n\
                   \nYour browser doesn't support HTML5 audio\
                   \n\
                   \n/\201\161ri\203\144k/\
                   \n\
                   \nUS\
                   \n\
                   \nYour browser doesn't support HTML5 audio\
                   \n\
                   \n/\201\161ri\203\144k/\
                   \n\
                   \nto put symbols or text that does not mean anything in the place of actual text in a document, on a page, etc., to show what the general design looks like or because the real text is not yet available\
                   \n\
                   \nAbout this\
                   \n\
                   \nBlog\
                   \n\
                   \nStumbling and tumbling (The language of falling)\
                   \n\
                   \nMarch 25, 2026\
                   \n\
                   \nRead More\
                   \n\
                   \npotato bed\" layout=\"responsive\">\
                   \n\
                   \nNew Words\
                   \n\
                   \npotato bed\
                   \n\
                   \nMarch 23, 2026\
                   \n\
                   \nMore new words\
                   \n\
                   \nhas been added to list\
                   \n\
                   \nTo top\
                   \n\
                   \nContents\
                   \n\
                   \nEnglishTranslations\
                   \n\
                   \n- Cambridge Dictionary +Plus\
                   \n- My profile\
                   \n- +Plus help\
                   \n- Log out\
                   \n\
                   \nEnglish (US) Change\
                   \n\
                   \nEnglish (UK) English (US) Espa\195\177ol Portugu\195\170s\228\184\173\230\150\135 (\231\174\128\228\189\147)\230\173\163\233\171\148\228\184\173\230\150\135 (\231\185\129\233\171\148) Dansk Deutsch Fran\195\167ais Italiano Nederlands Norsk Polski\208\160\209\131\209\129\209\129\208\186\208\184\208\185 T\195\188rk\195\167e Ti\225\186\191ng Vi\225\187\135t Svenska\208\163\208\186\209\128\208\176\209\151\208\189\209\129\209\140\208\186\208\176\230\151\165\230\156\172\232\170\158\237\149\156\234\181\173\236\150\180\224\170\151\224\171\129\224\170\156\224\170\176\224\170\190\224\170\164\224\171\128\224\174\164\224\174\174\224\174\191\224\174\180\224\175\141\224\176\164\224\177\134\224\176\178\224\177\129\224\176\151\224\177\129\224\166\172\224\166\190\224\166\153\224\167\141\224\166\151\224\166\190\224\166\178\224\166\191\224\164\174\224\164\176\224\164\190\224\164\160\224\165\128\224\164\185\224\164\191\224\164\130\224\164\166\224\165\128\
                   \n\
                   \nFollow us\
                   \n\
                   \nChoose a dictionary\
                   \n\
                   \nRecent and Recommended\
                   \n\
                   \nDefinitions\
                   \n\
                   \nClear explanations of natural written and spoken English\
                   \n\
                   \nEnglish Learner\226\128\153s Dictionary Essential British English Essential American English\
                   \n\
                   \nGrammar and thesaurus\
                   \n\
                   \nUsage explanations of natural written and spoken English\
                   \n\
                   \nGrammar Thesaurus\
                   \n\
                   \nPronunciation\
                   \n\
                   \nBritish and American pronunciations with audio\
                   \n\
                   \nEnglish Pronunciation\
                   \n\
                   \nTranslation\
                   \n\
                   \nClick on the arrows to change the translation direction.\
                   \n\
                   \nBilingual Dictionaries\
                   \n\
                   \n- English\226\128\147Chinese (Simplified) Chinese (Simplified)\226\128\147English\
                   \n- English\226\128\147Chinese (Traditional) Chinese (Traditional)\226\128\147English\
                   \n- English\226\128\147Danish Danish\226\128\147English\
                   \n- English\226\128\147Dutch Dutch\226\128\147English\
                   \n- English\226\128\147French French\226\128\147English\
                   \n- English\226\128\147German German\226\128\147English\
                   \n- English\226\128\147Indonesian Indonesian\226\128\147English\
                   \n- English\226\128\147Italian Italian\226\128\147English\
                   \n- English\226\128\147Japanese Japanese\226\128\147English\
                   \n- English\226\128\147Norwegian Norwegian\226\128\147English\
                   \n- English\226\128\147Polish Polish\226\128\147English\
                   \n- English\226\128\147Portuguese Portuguese\226\128\147English\
                   \n- English\226\128\147Spanish Spanish\226\128\147English\
                   \n- English\226\128\147Swedish Swedish\226\128\147English\
                   \n\
                   \nSemi-bilingual Dictionaries\
                   \n\
                   \nEnglish\226\128\147Arabic English\226\128\147Bengali English\226\128\147Catalan English\226\128\147Czech English\226\128\147Gujarati English\226\128\147Hindi English\226\128\147Korean English\226\128\147Malay English\226\128\147Marathi English\226\128\147Russian English\226\128\147Tamil English\226\128\147Telugu English\226\128\147Thai English\226\128\147Turkish English\226\128\147Ukrainian English\226\128\147Urdu English\226\128\147Vietnamese\
                   \n\
                   \nDictionary +Plus\
                   \n\
                   \nWord Lists\
                   \n\
                   \nContents\
                   \n\
                   \nEnglish\
                   \n\
                   \nNoun\
                   \n\
                   \n- Translations\
                   \n- Grammar\
                   \n- All translations\
                   \n\
                   \nMy word lists\
                   \n\
                   \nTo add Paris to a word list please sign up or log in.\
                   \n\
                   \nSign up or Log in\
                   \n\
                   \nMy word lists\
                   \n\
                   \nAdd Paris to one of your lists below, or create a new one.\
                   \n\
                   \n5 && !stateSidebarWordList.expended) ? 195 : (stateSidebarWordListItems.length * 39)\" [src]=\"stateSidebarWordListItems\">\
                   \n\
                   \n5 && !stateSidebarWordList.expended) ? 'hao hp lmt-25' : 'hdn'\">\
                   \n\
                   \nMore\
                   \n\
                   \nGo to your word lists\
                   \n\
                   \nTell us about this example sentence:\
                   \n\
                   \nThe word in the example sentence does not match the entry word.\
                   \n\
                   \nThe sentence contains offensive content.\
                   \n\
                   \nCancel Submit\
                   \n\
                   \nThe word in the example sentence does not match the entry word.\
                   \n\
                   \nThe sentence contains offensive content.\
                   \n\
                   \nCancel Submit")))))))
            (Object
             ((type (String url_citation))
              (url_citation
               (Object
                ((url (String https://britannica.com/place/Paris))
                 (title
                  (String
                   "Paris | Definition, Map, Population, Facts, & History | Britannica"))
                 (start_index (Number 0)) (end_index (Number 0))
                 (content
                  (String
                    "Paris | Definition, Map, Population, Facts, & History | Britannica\
                   \n\
                   \nParis; Eiffel Tower View of the Paris skyline from Montparnasse. (more)\
                   \n\
                   \n# Paris\
                   \n\
                   \nnational capital, France\
                   \n\
                   \nAsk Anything Quick Summary Homework Help\
                   \n\
                   \nAlso known as: Lutetia\
                   \n\
                   \nWritten by\
                   \n\
                   \nJohn Anthony Charles Ardagh All\
                   \n\
                   \nFact-checked by\
                   \n\
                   \nBritannica Editors\
                   \n\
                   \nLast updated\
                   \n\
                   \nMay 7, 2026 \226\128\162 History\
                   \n\
                   \nBritannica AI\
                   \n\
                   \nAsk Anything Quick Summary\
                   \n\
                   \nTable of Contents\
                   \n\
                   \nTable of Contents Quick Summary Ask Anything\
                   \n\
                   \nTop Questions\
                   \n\
                   \n### Where is Paris located?\
                   \n\
                   \nParis is located in the north-central part of France along the Seine River. It is at the center of the \195\142le-de-France region.\
                   \n\
                   \n### What is the weather like in Paris?\
                   \n\
                   \nParis weather can be very changeable. The wind can be sharp and cold in winter and spring. The annual average temperature is in the lower 50s \194\176F (about 12 \194\176C); the July average is in the upper 60s \194\176F (about 19 \194\176C), and the January average is in the upper 30s \194\176F (about 3 \194\176C).\
                   \n\
                   \n### What is the landscape of Paris?\
                   \n\
                   \nParis occupies a depression hollowed out by the Seine. The surrounding heights have elevations that vary from 430 feet (130 meters), at the butte of Montmartre in the north, to 85 feet (26 meters), in the Grenelle area in the southwest. The city is surrounded by great forests of beech and oak, called the \226\128\156lungs of Paris,\226\128\157 as they help purify the air in the region.\
                   \n\
                   \n### Paris is the capital of what country?\
                   \n\
                   \nParis is the national capital of France.\
                   \n\
                   \n## News \226\128\162\
                   \n\
                   \nFrench prosecutors seek charges against Elon Musk and X over child sexual abuse images\226\128\162 May 7, 2026, 3:13 PM ET (AP) ...(Show more)\
                   \n\
                   \nParis Saint-Germain returns to Champions League final as Demb\195\169l\195\169 goal stifles Bayern Munich\226\128\162 May 6, 2026, 6:36 PM ET (AP)\
                   \n\
                   \nFrance reckons with Nazi-looted art in a new Paris museum gallery\226\128\162 May 5, 2026, 10:58 PM ET (AP)\
                   \n\
                   \nMidfielder Vitinha starts for PSG against Bayern Munich in Champions League semifinal\226\128\162 Apr. 28, 2026, 2:11 PM ET (AP)\
                   \n\
                   \nParis offers to host 2030 Winter Games ice hockey after Nice mayor opposes Olympic plan\226\128\162 Apr. 23, 2026, 11:29 AM ET (AP)\
                   \n\
                   \nShow less\
                   \n\
                   \nParis, city and capital of France, situated in the north-central part of the country. People were living on the site of the present-day city, located along the Seine River some 233 miles (375 km) upstream from the river\226\128\153s mouth on the English Channel(La Manche), by about 7600 bce. The modern city has spread from the island (the \195\142le de la Cit\195\169) and far beyond both banks of the Seine.\
                   \n\
                   \nParis occupies a central position in the rich agricultural region known as the Paris Basin, and it constitutes one of eight d\195\169partements of the\195\142le-de-France administrative region. It is by far the country\226\128\153s most important centre of commerce and culture. Area city, 41 square miles (105 square km); metropolitan area, 890 square miles (2,300 square km). Pop. (2020 est.) city, 2,145,906; (2020 est.) urban agglomeration, 10,858,874.\
                   \n\
                   \n## Character of the city\
                   \n\
                   \nFor centuries Paris has been one of the world\226\128\153s most important and attractive cities. It is appreciated for the opportunities it offers for business and commerce, for study, for culture, and for entertainment; its gastronomy, haute couture, painting, literature, and intellectual community especially enjoy an enviable reputation. Its sobriquet\226\128\156the City of Light\226\128\157 (\226\128\156la Ville Lumi\195\168re\226\128\157), earned during the Enlightenment, remains appropriate, for Paris has retained its importance as a centre for education and intellectual pursuits.\
                   \n\
                   \nParis\226\128\153s site at a crossroads of both water and land routes significant not only to France but also to Europe has had a continuing influence on its growth. Under Roman administration, in the 1st century bce, the original site on the \195\142le de la Cit\195\169 was designated the capital of the Parisii tribe and territory. The Frankish king Clovis I had taken Paris from the Gauls by 494 ce and later made his capital there. Under Hugh Capet(ruled 987\226\128\147996) and the Capetian dynasty the preeminence of Paris was firmly established, and Paris became the political and cultural hub as modern France took shape. France has long been a highly centralized country, and Paris has come to be identified with a powerful central state, drawing to itself much of the talent and vitality of the provinces.\
                   \n\
                   \nParis, France The Seine River flows past the \195\142le Saint-Louis in Paris, France.(more)\
                   \n\
                   \nThe three main parts of historical Paris are defined by the Seine. At its centre is the \195\142le de la Cit\195\169, which is the seat of religious and temporal authority (the word cit\195\169 connotes the nucleus of the ancient city). The Seine\226\128\153s Left Bank (Rive Gauche) has traditionally been the seat of intellectual life, and its Right Bank (Rive Droite) contains the heart of the city\226\128\153s economic life, but the distinctions have become blurred in recent decades. The fusion of all these functions at the centre of France and, later, at the centre of an empire, resulted in a tremendously vital environment. In this environment, however, the emotional and intellectual climate that was created by contending powers often set the stage for great violence in both the social and political arenas\226\128\148the years 1358, 1382, 1588, 1648, 1789, 1830, 1848, and 1871 being notable for such events.\
                   \n\
                   \nBritannica QuizGuess the City by Its River Quiz\
                   \n\
                   \nmap of Paris c. 1900 Map of Paris, c. 1900, from the 10th edition of Encyclop\195\166dia Britannica.(more)\
                   \n\
                   \nIn its centuries of growth Paris has for the most part retained the circular shape of the early city. Its boundaries have spread outward to engulf the surrounding towns (bourgs), usually built around monasteries or churches and often the site of a market. From the mid-14th to the mid-16th century, the city\226\128\153s growth was mainly eastward; since then it has been westward. It comprises 20 arrondissements(municipal districts), each of which has its own mayor, town hall, and particular features. The numbering begins in the heart of Paris and continues in the spiraling shape of a snail shell, ending to the far east. Parisians refer to the arrondissements by number as the first (premier), second (deuxi\195\168me), third (troisi\195\168me), and so on. Adaptation to the problems of urbanization\226\128\148such as immigration, housing, social infrastructure, public utilities, suburban development, and zoning\226\128\148has produced the vast urban agglomeration.\
                   \n\
                   \nThe Editors of Encyclopaedia Britannica\
                   \n\
                   \n## Landscape\
                   \n\
                   \n## City site\
                   \n\
                   \nParis Paris and its metropolitan area.(more)\
                   \n\
                   \nParis is positioned at the centre of the \195\142le-de-France region, which is crossed by the Seine, Oise, and Marne rivers. The city is ringed with great forests of beech and oak; they are called the \226\128\156lungs of Paris,\226\128\157 for they help to purify the air in the heavily industrialized region. The city proper is small; no corner is farther than about 6 miles (10 km) from the square in front of Notre-Dame Cathedral. It occupies a depression hollowed out by the Seine, and the surrounding heights have been respected as the limits of the city. Elevation varies from 430 feet (130 metres) at the butte of Montmartre, in the north, to 85 feet (26 metres) in the Grenelle area, in the southwest.\
                   \n\
                   \nExplore Britannica Premium!\
                   \n\
                   \nTrusted knowledge for those who want to know more.\
                   \n\
                   \nThe Seine flows for about 8 miles (13 km) through the centre of the city and 10 of the 20 arrondissements. It enters the city at the southeast corner, flows northwestward, and turns gradually southwestward, eventually leaving Paris at the southwest corner. As a result, what starts out as the stream\226\128\153s east bank becomes its north bank and ends as the west bank, and the Parisians therefore adopted the simple, unchanging designation of Right Bank and Left Bank (when facing downstream). Specific places, however, are usually indicated by arrondissement or by quarter (quartier).\
                   \n\
                   \nAt water level, some 30 feet (9 metres) below street level, the river is bordered\226\128\148at least on those portions not transformed into expressways\226\128\148by cobbled quays graced with trees and shrubs. From street level another line of trees leans toward the water. Between the two levels, the retaining walls, usually made of massive stone blocks, are decorated with the great iron rings once used to moor merchant vessels, and some are pierced by openings left by water gates for old palaces or inspection ports for subways, sewers, and underpasses. At intermittent points the walls are shawled in ivy.\
                   \n\
                   \nThe garden effect of the Seine\226\128\153s open waters and its tree-lined banks foster in part the appearance of Paris as a city well-endowed with green spaces. Tens of thousands of trees (mostly plane trees, with a scattering of chestnuts) line the streets as well, and numerous public parks, gardens, and squares dot the city. Most of the parks and gardens of the modern central city are on land that formerly was reserved for the kings on the old city\226\128\153s outskirts. Under Napoleon III, who had been impressed by London\226\128\153s parks while living in Britain, two ancient royal military preserves at the approaches to Paris were made into \226\128\156English\226\128\157 parks\226\128\148the Bois de Boulogne to the west and the Bois de Vincennes to the east. Moreover, during his reign a large area of land was laid out in promenades and garden squares. Under Mayor Jacques Chirac in the late 20th century, the municipal government initiated efforts to create new parks, and such projects continued into the 21st century.\
                   \n\
                   \nThe Promenade Plant\195\169e is a partially elevated parkway built along an abandoned rail line and viaduct in the 12th arrondissement (municipal district) of Paris, on the right bank of the Seine River. It was the world\226\128\153s first elevated park (first phase completed in 1994) and the first \226\128\156green space\226\128\157 constructed on a viaduct; it has since inspired other cities to turn abandoned rail lines into public parkland. The entire feature runs some 4.5 km (about 3 miles) from the Op\195\169ra Bastille to the Bois de Vincennes. Located underneath the elevated portion is the Viaduc des Arts, which stretches along the Avenue Daumesnil. Its former archways house specialized commercial establishments.")))))))
            (Object
             ((type (String url_citation))
              (url_citation
               (Object
                ((url
                  (String
                   https://worldpopulationreview.com/countries/france/capital))
                 (title (String "What is the Capital of France?"))
                 (start_index (Number 0)) (end_index (Number 0))
                 (content
                  (String
                    "What is the Capital of France?\
                   \n# What is the Capital of France?\
                   \nThe capital ofFranceisParis, which was founded3rd century.Parishas been the capital since1944.\
                   \nParis is the largest city in France and functions as: Houses the political and economical Luxembour.\
                   \nParisis located at48.8566\194\176 N, 2.3522\194\176 E, at an elevation of115'ft.\
                   \n### Eiffel Tower, Paris, France\
                   \n![France Capital](https://s3.amazonaws.com/images.wpr.com/capitals/720/27029805017_250c54b3ed_o.jpg)\
                   \nSource:[https://flic.kr/p/HbwPjZ](https://flic.kr/p/HbwPjZ)\
                   \n2024 Population\
                   \n11,276,701\
                   \n2024 Growth Rate\
                   \n0.61%\
                   \nFounded\
                   \n3rd century\
                   \nCapital as of\
                   \n1944\
                   \nElevation\
                   \n115'\
                   \nLargest City\
                   \nYes\
                   \nRole in Country\
                   \nHouses the political and economical Luxembour\
                   \n## Paris Shown on a Map")))))))
            (Object
             ((type (String url_citation))
              (url_citation
               (Object
                ((url (String https://merriam-webster.com/dictionary/Paris))
                 (title (String "PARIS Definition & Meaning - Merriam-Webster"))
                 (start_index (Number 0)) (end_index (Number 0))
                 (content
                  (String
                    "PARIS Definition & Meaning - Merriam-Webster (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start': new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0], j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src= 'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f); })(window,document,'script','dataLayer', 'GTM-WW4KHXF');--\
                   \n\
                   \nDictionary\
                   \n\
                   \n- Related Articles\
                   \n- Entries Near\
                   \n- Rhymes\
                   \n- Phrases Containing\
                   \n- Word History\
                   \n- noun\
                   \n- noun\
                   \n\
                   \n- More from M-W\
                   \n- Biographical NameBiographical\
                   \n- Geographical NameGeographical\
                   \n- Cite this EntryCitation\
                   \n\
                   \nShow more\
                   \n\
                   \n- More from M-W\
                   \n- Biographical\
                   \n- Geographical\
                   \n- Citation\
                   \n\
                   \nSave Word\
                   \n\
                   \nTo save this word, you'll need to log in.\
                   \n\
                   \nLog In\
                   \n\
                   \n# Paris\
                   \n\
                   \n## noun\
                   \n\
                   \nPar\194\183\226\128\139is\203\136per-\201\153s\
                   \n\
                   \n\203\136pa-r\201\153s\
                   \n\
                   \nSave Word --\
                   \n\
                   \n: a son of Priam whose abduction of Helen leads to the Trojan War\
                   \n\
                   \nhttps://merriam.atlassian.net/browse/MWSITE-9211 --\
                   \n\
                   \nLove words? Need even more definitions?\
                   \n\
                   \nSubscribe to America's largest dictionary and get thousands more definitions and advanced search\226\128\148ad free!\
                   \n\
                   \nMerriam-Webster unabridged\
                   \n\
                   \n--\
                   \n\
                   \n## More from Merriam-Webster\
                   \n\
                   \n#### Word of the Day\
                   \n\
                   \n#### eureka\
                   \n\
                   \nSee Definitions and Examples\194\187\
                   \n\
                   \nGet Word of the Day daily email!\
                   \n\
                   \n--\
                   \n\
                   \n-- --\
                   \n\
                   \n## Popular in Grammar & Usage\
                   \n\
                   \nSee More\
                   \n\
                   \n### 5 Verbal Slip Ups and Language Mistakes\
                   \n\
                   \n### What does 'etcetera' mean?\
                   \n\
                   \n### Is it 'nerve-racking' or 'nerve-wracking'?\
                   \n\
                   \n### The Difference Between 'i.e.' and 'e.g.'\
                   \n\
                   \n### Democracy or Republic: What's the difference?\
                   \n\
                   \nSee More\
                   \n\
                   \n## Popular in Wordplay\
                   \n\
                   \nSee More\
                   \n\
                   \n### The Opposite of D\195\169j\195\160 Vu\
                   \n\
                   \n### Why do we say 'fly off the handle'?\
                   \n\
                   \n### Why do we say 'get out of Dodge'?\
                   \n\
                   \n### The Words of the Week - Mar. 13\
                   \n\
                   \n### Why do we call it a 'flea market'?\
                   \n\
                   \nSee More\
                   \n\
                   \n## Popular\
                   \n\
                   \nSee More\
                   \n\
                   \n### 5 Verbal Slip Ups and Language Mistakes\
                   \n\
                   \n### The Opposite of D\195\169j\195\160 Vu\
                   \n\
                   \n### Why do we say 'fly off the handle'?\
                   \n\
                   \nSee More\
                   \n\
                   \n## Games & Quizzes\
                   \n\
                   \nSee All\
                   \n\
                   \nQuordle Can you solve 4 words at once? Play\
                   \n\
                   \nBlossom Pick the best words! Play\
                   \n\
                   \nThe Missing Letter A daily crossword with a twist Play\
                   \n\
                   \nFamous Novels, First Lines Quiz Can you identify these novels by their famous fir... Take the quiz\
                   \n\
                   \nSee All")))))))
            (Object
             ((type (String url_citation))
              (url_citation
               (Object
                ((url
                  (String
                   https://www.factmonster.com/encyclopedia/places/west-europe/france/paris-city-france))
                 (title (String "Paris, city, France | FactMonster"))
                 (start_index (Number 0)) (end_index (Number 0))
                 (content
                  (String
                    "Paris, city, France | FactMonster\
                   \n\
                   \n1. / Paris, city, France\
                   \n2. / French Political Geography\
                   \n3. / Britain, Ireland, France, and the Low Countries\
                   \n4. / Places\
                   \n5. / Columbia Encyclopedia\
                   \n6. Teaching Resources\
                   \n\
                   \n# Paris, city, France\
                   \n\
                   \nEnter your search terms:\
                   \n\
                   \n### Introduction\
                   \n\
                   \nParis p\195\162r\203\136\196\173s, Fr. p\195\164r\196\147\203\136 [key], city (1999 pop. 2,115,757; metropolitan area est. pop. 11,000,000), N central France, capital of the country, on the Seine River. It is the commercial and industrial focus of France and a cultural and intellectual center of international renown. The city possesses an indefinable unity of atmosphere that has fascinated writers, poets, and painters for centuries. Paris is sometimes called the City of Light in tribute to its intellectual preeminence as well as to its beautiful appearance.\
                   \n\
                   \nParis is the center of many major newspapers and periodicals, as well as all the major French radio and television stations. Elegant stores and hotels, lavish nightclubs, theaters, and gourmet restaurants help make tourism the biggest industry in Paris. Other leading industries manufacture luxury articles, high-fashion clothing, perfume, and jewelry. Heavy industry, notably automobile manufacture, is located in the suburbs. About one quarter of the French labor force is concentrated in the Paris area.\
                   \n\
                   \n#### Sections in this article:\
                   \n\
                   \n- Bibliography\
                   \n- Transportation Facilities\
                   \n- Points of Interest\
                   \n- Government and People\
                   \n- Early History\
                   \n- During the Renaissance\
                   \n- The Seventeenth and Eighteenth Centuries\
                   \n- Napoleon to the Commune\
                   \n- Under the Third Republic\
                   \n- Contemporary Paris\
                   \n- Introduction\
                   \n\
                   \n### Contemporary Paris\
                   \n\
                   \nParis was the headquarters of NATO from 1950 to 1967; it is the headquarters of UNESCO and the European Space Agency. A program of cleaning the city's major buildings and monuments was completed in the 1960s. The city was the scene in May, 1968, of serious disorders, beginning with a student strike, that nearly toppled the Fifth Republic. In 1971, Les Halles, Paris's famous central market, called by Zola the \226\128\156belly\226\128\157 of Paris, was dismantled. Construction began immediately on Chatelet Les-Halles, Paris's new metro hub, which was completed in 1977. The Forum des Halles, a partially underground, multistory commercial and shopping center, opened in 1979. Other developments include the Georges Pompidou National Center for Art and Culture, built in 1977, which includes the National Museum of Modern Art. The Louvre underwent extensive renovation, and EuroDisney, a multibillion dollar theme and amusement park, opened in the Parisian suburbs in 1992. A number of major projects in the city were initiated by President Fran\195\167ois Mitterrand(1981\226\128\14795); they include the new Biblioth\195\168que Nationale, the glass pyramid at the Louvre, Grande Arche de la D\195\169fense, Arab Institute, Bastille Opera, and Cit\195\169 de la Musique.\
                   \n\
                   \n### Under the Third Republic\
                   \n\
                   \nWith the establishment of the Third French Republic and relative stability, Paris became the great industrial and transportation center it is today. Two epochal events in modern cultural history that took place in Paris were the first exhibition of impressionist painting (1874) and the premiere of Stravinsky's Sacre du Printemps (1913). In World War I the Germans failed to reach Paris. After 1919 the outermost city fortifications were replaced by housing developments, including the Cit\195\169 Universitaire, which houses thousands of students. During the 1920s, a time of enormous cultural and social activity, Paris was home to many artists and writers from the United States and elsewhere. German troops occupied Paris during World War II from June 14, 1940, to Aug. 25, 1944. The city was not seriously damaged by the war.\
                   \n\
                   \n### Napoleon to the Commune\
                   \n\
                   \nNapoleon (emperor, 1804\226\128\14715) began a large construction program (including the building of the Arc de Triomphe, the Vend\195\180me Column, and the arcaded Rue de Rivoli) and enriched the city's museums with artworks removed from conquered cities. In the course of his downfall Paris was occupied twice by enemy armies (1814, 1815). In the first half of the 19th cent. Paris grew rapidly. In 1801 it had 547,000 people; in 1817, 714,000; in 1841, 935,000; and in 1861, 1,696,000. The revolutions of July, 1830, and Feb., 1848, both essentially Parisian events, had repercussions throughout Europe. Culturally, the city was at various times the home or host of most of the great European figures of the age. Balzac, Hugo, Chopin, Berlioz, Liszt, Wagner, Delacroix, Ingres, and Daumier were a few of the outstanding personalities. The grand outline of modern Paris was the work of Baron Georges Haussmann, who was appointed prefect by Napoleon III. The great avenues, boulevards, and parks are his work. During the Franco-Prussian War (1870\226\128\14771), Paris was besieged for four months by the Germans and then surrendered. After the Germans withdrew, Parisian workers rebelled against the French government and established the Commune of Paris, which was bloodily suppressed.\
                   \n\
                   \n### The Seventeenth and Eighteenth Centuries\
                   \n\
                   \nDuring the late 17th and the 18th cent. Paris acquired further glory as the scene of many of France's greatest cultural achievements: the plays of Moli\195\168re, Racine, and Corneille; the music of Lully, Rameau, and Gluck; the paintings of Watteau, Fragonard, and Boucher; and the salons where many of the philosophes of the Enlightenment gathered. At the same time, growing industries had resulted in the creation of new classes\226\128\148the bourgeoisie and proletariat\226\128\148concentrated in such suburbs (faubourgs) as Saint-Antoine and Saint-Denis; in the opening events of the French Revolution, city mobs stormed the Bastille (July, 1789) and hauled the royal family from Versailles to Paris (Oct., 1789). Throughout the turbulent period of the Revolution the city played a central role.\
                   \n\
                   \n### During the Renaissance\
                   \n\
                   \nThe Renaissance reached Paris in the 16th cent. during the reign of Francis I (1515\226\128\14747). At this time the Louvre was transformed from a fortress to a Renaissance palace. In the Wars of Religion (1562\226\128\14798), Parisian Catholics, who were in the great majority, took part in the massacre of St. Bartholomew's Day (1572), forced Henry III to leave the city on the Day of Barricades (1588), and accepted Henry IV only after his conversion (1593) to Catholicism. Cardinal Richelieu, Louis XIII's minister, established the French Academy and built the Palais Royal and the Luxembourg Palace. During the Fronde, Paris once again defied the royal authority. Louis XIV, distrustful of the Parisians, transferred (1682) his court to Versailles. Parisian industries profited from the lavishness of Versailles; the specialization in luxury goods dates from that time. J. H. Mansart under Louis XIV and Fran\195\167ois Mansart, J. G. Soufflot, and J. A. Gabriel under Louis XV created some of the most majestic prospects of modern Paris.\
                   \n\
                   \n### Early History\
                   \n\
                   \nJulius Caesar conquered Paris in 52 b.c. It was then a fishing village, called Lutetia Parisiorum (the Parisii were a Gallic tribe), on the \195\142le de la Cit\195\169. Under the Romans the town spread to the left bank and acquired considerable importance under the later emperors. The vast catacombs under Montparnasse and the baths (now in the Cluny Mus.) remain from the Roman period. Legend says that St. Denis, first bishop of Paris, was martyred on Montmartre (hence the name) and that in the 5th cent. St. Genevi\195\168ve, the patron saint of Paris, preserved the city from destruction by the Huns. On several occasions in its early history Paris was threatened by barbarian and Norman invasions, which at times drove the inhabitants back to the \195\142le de la Cit\195\169.\
                   \n\
                   \nClovis I and several other Merovingian kings made Paris their capital; under Charlemagne it became a center of learning. In 987, Hugh Capet, count of Paris, became king of France. The Capetians firmly established Paris as the French capital. The city grew as the power of the French kings increased. In the 11th cent. the city spread to the right bank. During the next two centuries\226\128\148the reign of Philip Augustus (1180\226\128\1471223) is especially notable for the growth of Paris\226\128\148streets were paved and the city walls enlarged; the first Louvre (a fortress) and several churches, including Notre-Dame, were constructed or begun; and the schools on the left bank were organized into the Univ. of Paris. One of them, the Sorbonne, became a fountainhead of theological learning with Albertus Magnus and St. Thomas Aquinas among its scholars. The university community constituted an autonomous borough; another was formed on the right bank by merchants ruled by their own provost. In 1358, under the leadership of the merchant provost \195\137tienne Marcel, Paris first assumed the role of an independent commune and rebelled against the dauphin (later Charles V). During the period of the Hundred Years War the city suffered civil strife (see Armagnacs and Burgundians), occupation by the English (1419\226\128\14736), famine, and the Black Death.\
                   \n\
                   \n### Government and People\
                   \n\
                   \nParis is divided into 20 arrondissements (districts or boroughs), each of which has a local council and a mayor, but most of the power is held by the mayor of the City of Paris who is chosen by the city's council. Paris and its suburbs together make up the eight departments of the \195\142le-de-France administrative region, which is governed by an elected assembly, chairman (or president), and supervisor and overseen by a prefect appointed by the state.\
                   \n\
                   \nImmigrants to France now constitute nearly 20% of Paris's population. The majority of these are Algerian, Moroccan, and Tunisian. Large groups of Indochinese have also immigrated to Paris. About 75% of all Parisians live in the suburbs due to high costs and a high population density in the city. New towns have been built, consolidating suburban areas, and a great deal of manufacturing and other industry takes place in the suburbs.\
                   \n\
                   \n### Points of Interest\
                   \n\
                   \nParis is divided into roughly equal sections by the Seine. On the right (northern) bank are the Bois de Boulogne and the adjoining Stade Roland Garros (site of tennis's French Open), Arc de Triomphe, the old Biblioth\195\168que nationale,\195\137lys\195\169e Palace, Grand Palais, Georges Pompidou National Center for Art and Culture (see Beaubourg), Place de la Concorde, Op\195\169ra, Com\195\169die Fran\195\167aise, Louvre, Palais de Chaillot, Maison Europ\195\169enne de la Photographie, Grande Arche de la D\195\169fense, Champs \195\137lys\195\169es, and other great streets, sites, and boulevards. In the eastern part of the right bank is the Museum of the Art and History of Judaism, the Place de la Bastille and the Bastille Opera; to the north is Montmartre, the highest area in Paris, topped by the Church of Sacr\195\169-C\197\147ur. Much of the right bank, which has many of the most fashionable streets and shops, has a stately air. At night many monuments and boulevards are floodlit. In the city's northeastern outskirts is the Parc de la Villette, home of the Cit\195\169 des Sciences et de l'Industrie (1986), the Cit\195\169 de la Musique (1995), the Philharmonie de Paris (2015), and other performance and exhibition spaces.\
                   \n\
                   \nThe left bank, with the Sorbonne, the French Academy, the Panth\195\169on (see under pantheon), the Luxembourg Palace and Gardens, the Jardin des Plantes (site of the National Natural History Museum), the Chamber of Deputies, the Quai d'Orsay, and the Hotel des Invalides, is the governmental and to a large extent the intellectual section. The Latin Quarter, for nearly a thousand years the preserve of university students and faculty; the Faubourg Saint-Germain section, at once aristocratic and a haven for students and artists (the celebrated Caf\195\169 des Deux Magots and Caf\195\169 de Flore are there); and Montparnasse are the most celebrated left-bank districts. The Eiffel Tower stands by the Seine on the Champ-de-Mars. In SE Paris, also on the left bank, is Paris Rive Gauche, a former industrial area redeveloped with a variety of newer buildings and renovations, many by prominent architects; the new Biblioth\195\168que nationale (opened 1998) is there.\
                   \n\
                   \nThe historical nucleus of Paris is the \195\142le de la Cit\195\169, a small boat-shaped island largely occupied by the huge Palais de Justice and the Cathedral of Notre-Dame de Paris (damaged by fire, 2019). It is connected with the smaller \195\142le Saint-Louis, occupied by elegant houses of the 17th and 18th cent. Characteristic of Paris are the tree-lined quays along the Seine (famed, on the left bank, for their open-air bookstalls), the historic bridges that span the Seine, and the vast tree-lined boulevards that replaced the city walls. Skyscrapers, apartment complexes, and highways have been added to the Paris scene in recent years.\
                   \n\
                   \n### Transportation Facilities\
                   \n\
                   \nSituated in the center of the Paris basin (see\195\142le-de-France), and only 90 mi (145 km) from the English Channel, the city handles a great volume of shipping. Orly and Charles de Gaulle airports (the latter opened in 1974) and many major railroad stations make Paris one of the great transportation centers of western Europe. The Paris metro (subway), built in 1900, was modernized and extended during the 1970s. There are now 16 principal metro lines and a high-speed express subway system servicing the suburbs. The system's hub, Chatelet Les-Halles, is perhaps the largest, busiest underground station in the world. Paris is also the hub of the national rail system, with high-speed trains connecting it to most major European cities.\
                   \n\
                   \n### Bibliography\
                   \n\
                   \nSee J. Flanner, Paris Journal (2 vol., 1965\226\128\14771; repr. 1977) and Paris Was Yesterday, 1925\226\128\14739 (1988); A. Horne, The Fall of Paris: The Siege and the Commune, 1870\226\128\14771 (1965) and Seven Ages of Paris (2002); M. Kessel, The History of Paris, from Caesar to Saint Louis (tr. 1969); L. Bernard, The Emerging City: Paris in the Age of Louis XIV (1970); M. Guerrini, Napoleon and Paris: Thirty Years of History (tr. and abr. 1971); D. Thomson, Renaissance Paris (1984); D. Roche, The People of Paris (1987); J. Seigel, Bohemian Paris (1987); J.-M. P\195\169rouse de Montclos, Paris: City of Art (2003); S. Roux, Paris in the Middle Ages (2009); J. W. Baldwin, Paris 1200 (2010); E. Hazan, The Invention of Paris (2010); G. Robb, An Adventure History of Paris (2010); C. Rearick, Paris Dreams, Paris Memories (2011); S. Kirkland, Paris Reborn (2013); J. DeJean, How Paris Became Paris (2014); M. McAuliffe, Twilight of the Belle Epoque (2014) and When Paris Sizzled (2016); R. Christiansen, City of Light (2018).\
                   \n\
                   \nThe Columbia Electronic Encyclopedia, 6th ed. Copyright \194\169 2026, Columbia University Press. All rights reserved.\
                   \n\
                   \nSee more Encyclopedia articles on: French Political Geography\
                   \n\
                   \n\194\1692022 Sandbox Networks Inc. All rights reserved. Sandbox Learning is part of Sandbox & Co., a digital learning company.")))))))))
          (tool_calls ()) (tool_call_id ()))))))
     (system_fingerprint (fp_eb37e061ec)) (service_tier ())
     (usage
      ((prompt_tokens 7768) (completion_tokens 3) (total_tokens 7771)
       (cost (0.021167)) (is_byok (false))
       (prompt_tokens_details
        (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
          (video_tokens (0)))))
       (cost_details
        (((upstream_inference_cost (0.001167))
          (upstream_inference_prompt_cost (0.0011652))
          (upstream_inference_completions_cost (1.8E-06)))))
       (completion_tokens_details
        (((reasoning_tokens (0)) (image_tokens (0)) (audio_tokens (0)))))
       (server_tool_use ()))))
    |}];
  Deferred.unit
;;

(* Image output (Gemini): [message.images] populated with one entry whose
   [image_url.url] is a [data:image/png;base64,...] URL. The Gemini response
   shape omits the [index] field on the image object, which we tolerate. *)
let%expect_test "image_output" =
  let%bind () = parse_response_fixture "image_output" in
  [%expect
    {|
    ((id gen-1778404862-06EJO4Vy9kcQvNZYFzFN) (provider Google)
     (model google/gemini-2.5-flash-image) (object_ chat.completion)
     (created 1778404862)
     (choices
      (((logprobs ()) (finish_reason stop) (native_finish_reason STOP) (index 0)
        (message
         ((role assistant) (content ("Here you go: ")) (refusal ())
          (reasoning ()) (reasoning_details ())
          (images
           (((type_ image_url)
             (image_url
              ((url
                "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABAAAAAQACAIAAADwf7zUAAAAiXpUWHRSYXcgcHJvZmlsZSB0eXBlIGV4aWYAAHj...")))
             (index ()))))
          (annotations ()) (tool_calls ()) (tool_call_id ()))))))
     (system_fingerprint ()) (service_tier (standard))
     (usage
      ((prompt_tokens 5) (completion_tokens 1295) (total_tokens 1300)
       (cost (0.038714)) (is_byok (false))
       (prompt_tokens_details
        (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
          (video_tokens (0)))))
       (cost_details
        (((upstream_inference_cost (0.038714))
          (upstream_inference_prompt_cost (1.5E-06))
          (upstream_inference_completions_cost (0.0387125)))))
       (completion_tokens_details
        (((reasoning_tokens (0)) (image_tokens (1290)) (audio_tokens (0)))))
       (server_tool_use ()))))
    |}];
  Deferred.unit
;;

(* Streaming, simple text: a sequence of chunks consisting of a role-only
   first chunk, content-delta chunks, and a final stop chunk. *)
let%expect_test "stream_anthropic_haiku" =
  let%bind () = parse_stream_fixture "stream_anthropic_haiku" in
  [%expect
    {|
    (((id gen-1778404760-HzbZLh5i6tNjGlnRLzsQ) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404760)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (p)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404760-HzbZLh5i6tNjGlnRLzsQ) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404760)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (ong)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404760-HzbZLh5i6tNjGlnRLzsQ) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404760)
      (choices
       (((logprobs ()) (finish_reason (stop)) (native_finish_reason (end_turn))
         (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404760-HzbZLh5i6tNjGlnRLzsQ) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404760)
      (choices
       (((logprobs ()) (finish_reason (stop)) (native_finish_reason (end_turn))
         (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ())
      (usage
       (((prompt_tokens 13) (completion_tokens 5) (total_tokens 18)
         (cost (3.8E-05)) (is_byok (false))
         (prompt_tokens_details
          (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
            (video_tokens (0)))))
         (cost_details
          (((upstream_inference_cost (3.8E-05))
            (upstream_inference_prompt_cost (1.3E-05))
            (upstream_inference_completions_cost (2.5E-05)))))
         (completion_tokens_details
          (((reasoning_tokens (0)) (image_tokens (0)) (audio_tokens (0)))))
         (server_tool_use ()))))))
    |}];
  Deferred.unit
;;

(* Streaming with reasoning: chunks carry incremental [reasoning_details]
   entries with partial [text] / [signature]. *)
let%expect_test "stream_anthropic_reasoning" =
  let%bind () = parse_stream_fixture "stream_anthropic_reasoning" in
  [%expect
    {|
    (((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning (This))
           (reasoning_details
            (((format (anthropic-claude-v1)) (index (0)) (type_ (reasoning.text))
              (text (This)) (signature ()) (data ()))))
           (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ())
           (reasoning (" is a straight"))
           (reasoning_details
            (((format (anthropic-claude-v1)) (index (0)) (type_ (reasoning.text))
              (text (" is a straight")) (signature ()) (data ()))))
           (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ())
           (reasoning ("forward arithmetic"))
           (reasoning_details
            (((format (anthropic-claude-v1)) (index (0)) (type_ (reasoning.text))
              (text ("forward arithmetic")) (signature ()) (data ()))))
           (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ())
           (reasoning (" question. "))
           (reasoning_details
            (((format (anthropic-claude-v1)) (index (0)) (type_ (reasoning.text))
              (text (" question. ")) (signature ()) (data ()))))
           (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ())
           (reasoning ("1 + 1 = "))
           (reasoning_details
            (((format (anthropic-claude-v1)) (index (0)) (type_ (reasoning.text))
              (text ("1 + 1 = ")) (signature ()) (data ()))))
           (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning (2.))
           (reasoning_details
            (((format (anthropic-claude-v1)) (index (0)) (type_ (reasoning.text))
              (text (2.)) (signature ()) (data ()))))
           (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details
            (((format (anthropic-claude-v1)) (index (0)) (type_ (reasoning.text))
              (text ())
              (signature
               (EuMBCkgIDRABGAIqQBT3laJVTnu82MA+qIoM21rKNO7RavUnTB8hV2/jzC/qf/IkHKF+8sYptWVcO7SS3YeNr9R+77/331vMeJ5rnBUSDHmfEcEFvZ4NxlnXrRoMgUMHJQDVQQPGHZqmIjAxL0vdSycvENe6OtAw/BdZwR5+QTHW/cxWItq13SFnf0nu5n+AmDHv6mNx4sYGVsIqSQebHeU9PkpUtmYKYWtcCQZUXoJ7844EVTiVgGzy7G6LB/2a7mvcF/7ivM3oAcVFI9vhF2mFAB2IvVRFFvuoeOFJobt1TAfFZJkYAQ==))
              (data ()))))
           (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (1)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (" + 1 = **")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (2**)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason (stop)) (native_finish_reason (end_turn))
         (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ()) (usage ()))
     ((id gen-1778404762-tuLs4if5ACychoL5FgWw) (provider "Amazon Bedrock")
      (model anthropic/claude-4.5-haiku-20251001) (object_ chat.completion.chunk)
      (created 1778404762)
      (choices
       (((logprobs ()) (finish_reason (stop)) (native_finish_reason (end_turn))
         (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint ()) (service_tier ())
      (usage
       (((prompt_tokens 41) (completion_tokens 40) (total_tokens 81)
         (cost (0.000241)) (is_byok (false))
         (prompt_tokens_details
          (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
            (video_tokens (0)))))
         (cost_details
          (((upstream_inference_cost (0.000241))
            (upstream_inference_prompt_cost (4.1E-05))
            (upstream_inference_completions_cost (0.0002)))))
         (completion_tokens_details
          (((reasoning_tokens (15)) (image_tokens (0)) (audio_tokens (0)))))
         (server_tool_use ()))))))
    |}];
  Deferred.unit
;;

(* Streaming with [stream_options.include_usage = true]: an extra final
   chunk carries the [usage] field populated. *)
let%expect_test "stream_openai_with_usage" =
  let%bind () = parse_stream_fixture "stream_openai_with_usage" in
  [%expect
    {|
    (((id gen-1778404764-hvcUxQzjmt0eYSWjgRL8) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778404764)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (P)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ()) (usage ()))
     ((id gen-1778404764-hvcUxQzjmt0eYSWjgRL8) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778404764)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (ong)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ()) (usage ()))
     ((id gen-1778404764-hvcUxQzjmt0eYSWjgRL8) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778404764)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (!)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ()) (usage ()))
     ((id gen-1778404764-hvcUxQzjmt0eYSWjgRL8) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778404764)
      (choices
       (((logprobs ()) (finish_reason (stop)) (native_finish_reason (stop))
         (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ()) (usage ()))
     ((id gen-1778404764-hvcUxQzjmt0eYSWjgRL8) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778404764)
      (choices
       (((logprobs ()) (finish_reason (stop)) (native_finish_reason (stop))
         (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ())
      (usage
       (((prompt_tokens 11) (completion_tokens 4) (total_tokens 15)
         (cost (4.05E-06)) (is_byok (false))
         (prompt_tokens_details
          (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
            (video_tokens (0)))))
         (cost_details
          (((upstream_inference_cost (4.05E-06))
            (upstream_inference_prompt_cost (1.65E-06))
            (upstream_inference_completions_cost (2.4E-06)))))
         (completion_tokens_details
          (((reasoning_tokens (0)) (image_tokens (0)) (audio_tokens (0)))))
         (server_tool_use ()))))))
    |}];
  Deferred.unit
;;

let%expect_test "embeddings" =
  let%bind () = parse_embeddings_fixture "embeddings" in
  [%expect
    {|
    ((object_ list) (model text-embedding-3-small)
     (data
      (((object_ embedding) (index 0)
        (embedding
         (-0.0704345703125 -0.40625 0.353759765625 0.298095703125 -0.25732421875
          -0.435302734375 -0.314208984375 0.51171875)))))
     (usage ((prompt_tokens 2) (total_tokens 2) (cost (4E-08))))
     (provider (OpenAI)) (id (gen-emb-1778404857-ROxqLrvPOiI66GL9MNIr)))
    |}];
  Deferred.unit
;;

(* /generation endpoint shape: the wire format is [{"data": <Stats.t>}] — the
   helper unwraps [.data]. *)
let%expect_test "generation" =
  let%bind () = parse_generation_fixture "generation" in
  [%expect
    {|
    ((id gen-1778404767-XJNkUcdzAMdCArHue8Nn) (model openai/gpt-4o-mini)
     (provider_name (Azure)) (created_at 2026-05-10T09:19:27.555Z)
     (api_type (completions)) (origin ("")) (user_agent (ocaml-cohttp/v6.1.1))
     (http_referer ()) (session_id ())
     (request_id (req-1778404767-cGyksnLNZGDy2UKpprpv))
     (upstream_id (bd0a78c5-7812-44c2-a290-dd0dbd65af13)) (app_id ())
     (external_user ()) (router ()) (streamed (true)) (cancelled (false))
     (is_byok (false)) (finish_reason (stop)) (native_finish_reason (stop))
     (service_tier ()) (latency (489)) (moderation_latency ())
     (generation_time (648)) (tokens_prompt (2)) (tokens_completion (8))
     (native_tokens_prompt (11)) (native_tokens_completion (10))
     (native_tokens_completion_images ()) (native_tokens_reasoning (0))
     (native_tokens_cached (0)) (num_media_prompt ()) (num_input_audio_prompt ())
     (num_media_completion (0)) (num_search_results ()) (num_fetches ())
     (web_search_engine ()) (usage (7.65E-06)) (total_cost (7.65E-06))
     (upstream_inference_cost (0)) (cache_discount ())
     (response_cache_source_id ())
     (provider_responses
      (((endpoint_id (685e6cfb-ecef-4c83-8e2d-aa1ba667d744))
        (id (bd0a78c5-7812-44c2-a290-dd0dbd65af13)) (is_byok (false))
        (latency (376)) (model_permaslug (openai/gpt-4o-mini))
        (provider_name (Azure)) (status (200))))))
    |}];
  Deferred.unit
;;

(* /models is large (hundreds of entries × deeply-nested per-model records),
   so instead of snapshotting every model we verify the full body parses
   without error and snapshot a summary: total count + the first model in
   the list (for full-shape coverage of [Model_info.t]). *)
let%expect_test "models_list" =
  let%bind body = Reader.file_contents "responses/models_list.json" in
  let response =
    body |> Jsonaf.parse |> Or_error.ok_exn |> Models.For_testing.response_of_jsonaf
  in
  print_s
    [%message
      ""
        ~count:(List.length response.data : int)
        ~first:(List.hd_exn response.data : Models.Model_info.t)];
  [%expect
    {|
    ((count 367)
     (first
      ((id inclusionai/ring-2.6-1t:free)
       (canonical_slug inclusionai/ring-2.6-1t-20260508) (hugging_face_id ())
       (name "inclusionAI: Ring-2.6-1T (free)") (created 1778247440)
       (description
        ("Ring-2.6-1T is a 1T-parameter-scale thinking model with 63B active parameters, built for real-world agent workflows that require both strong capability and operational efficiency. It is optimized for coding agents, tool..."))
       (context_length (262144))
       (pricing
        ((prompt 0) (completion 0) (request ()) (image ()) (image_token ())
         (image_output ()) (audio ()) (input_audio_cache ()) (web_search ())
         (internal_reasoning ()) (input_cache_read ()) (input_cache_write ())
         (discount ())))
       (architecture
        ((tokenizer (Other)) (instruct_type ()) (modality (text->text))
         (input_modalities (text)) (output_modalities (text))))
       (top_provider
        ((context_length (262144)) (max_completion_tokens (65536))
         (is_moderated false)))
       (per_request_limits ())
       (supported_parameters
        (frequency_penalty include_reasoning max_tokens presence_penalty
         reasoning repetition_penalty seed stop temperature tool_choice tools
         top_k top_p))
       (default_parameters
        (((temperature ()) (top_p ()) (top_k ()) (frequency_penalty ())
          (presence_penalty ()) (repetition_penalty ()))))
       (knowledge_cutoff ()) (expiration_date ())
       (links
        (((details (/api/v1/models/inclusionai/ring-2.6-1t-20260508/endpoints))))))))
    |}];
  Deferred.unit
;;

(* Error envelope: [code] is a plain int, the body has an extra [user_id]
   field at top level (handled by [@@jsonaf.allow_extra_fields]). *)
let%expect_test "api_error_bad_model" =
  let%bind () = parse_api_error_fixture "api_error_bad_model" in
  [%expect
    {|
    ((error
      ((message "totally-not-a-model/foo is not a valid model ID")
       (code ((Int 400))) (metadata ()))))
    |}];
  Deferred.unit
;;
