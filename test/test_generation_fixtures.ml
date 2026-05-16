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
