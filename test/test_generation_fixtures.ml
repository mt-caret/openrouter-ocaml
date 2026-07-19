open! Core
open! Async
open Fixture_helpers

(* /generation endpoint shape: the wire format is [{"data": <Stats.t>}] — the
   helper unwraps [.data]. *)
let%expect_test "generation" =
  let%bind () = parse_generation_fixture "generation" in
  [%expect
    {|
    ((id gen-1778404767-XJNkUcdzAMdCArHue8Nn) (model openai/gpt-4o-mini)
     (provider_name (Azure)) (created_at 2026-05-10T09:19:27.555Z)
     (api_type (completions)) (origin ("")) (user_agent (ocaml-cohttp/v6.1.1))
     (http_referer ()) (session_id ()) (preset_id ())
     (request_id (req-1778404767-cGyksnLNZGDy2UKpprpv))
     (upstream_id (bd0a78c5-7812-44c2-a290-dd0dbd65af13)) (app_id ())
     (external_user ()) (router ()) (streamed (true)) (cancelled (false))
     (is_byok (false)) (finish_reason (stop)) (native_finish_reason (stop))
     (service_tier ()) (data_region ()) (latency (489)) (moderation_latency ())
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

(* Stats for a streamed request that wrote a prompt cache: [cache_discount] is
   negative (the cache-write surcharge), [data_region] is populated, and
   [provider_responses] records a rate-limited provider before the successful
   fallback. *)
let%expect_test "generation_cache_write" =
  let%bind () = parse_generation_fixture "generation_cache_write" in
  [%expect
    {|
    ((id gen-1784432434-AbxwtH7tu453ABv9aFni)
     (model anthropic/claude-4.6-sonnet-20260217) (provider_name (Anthropic))
     (created_at 2026-07-19T03:40:34.02Z) (api_type (completions)) (origin (""))
     (user_agent (curl/8.7.1)) (http_referer ()) (session_id ()) (preset_id ())
     (request_id (req-1784432434-0sbTGRydw5r9D4BX6mML))
     (upstream_id (msg_011CdAir2epbwEydHRzrcbWc)) (app_id ()) (external_user ())
     (router ()) (streamed (true)) (cancelled (false)) (is_byok (false))
     (finish_reason (stop)) (native_finish_reason (end_turn))
     (service_tier (default)) (data_region (global)) (latency (5739))
     (moderation_latency ()) (generation_time (5987)) (tokens_prompt (2559))
     (tokens_completion (1)) (native_tokens_prompt (2174))
     (native_tokens_completion (4)) (native_tokens_completion_images ())
     (native_tokens_reasoning (0)) (native_tokens_cached (0))
     (num_media_prompt ()) (num_input_audio_prompt ()) (num_media_completion (0))
     (num_search_results ()) (num_fetches ()) (web_search_engine ())
     (usage (0.008205)) (total_cost (0.008205)) (upstream_inference_cost (0))
     (cache_discount (-0.001623)) (response_cache_source_id ())
     (provider_responses
      (((endpoint_id (2054a2ee-87da-429b-ae50-8d727c24eef9)) (id ())
        (is_byok (false)) (latency (1017))
        (model_permaslug (anthropic/claude-4.6-sonnet-20260217))
        (provider_name (Google)) (status (429)))
       ((endpoint_id (b8603b8c-2a30-4ecb-848a-0eae45a80bb3))
        (id (msg_011CdAir2epbwEydHRzrcbWc)) (is_byok (false)) (latency (5735))
        (model_permaslug (anthropic/claude-4.6-sonnet-20260217))
        (provider_name (Anthropic)) (status (200))))))
    |}];
  Deferred.unit
;;
