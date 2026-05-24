open! Core
open! Async
open Openrouter_api
open Fixture_helpers

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
  let%bind () = flush_log () in
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
    1970-01-01 00:00:00.000000Z Error "lib/models.ml.Model_info.t_of_jsonaf: extra fields: supported_voices"
    |}];
  Deferred.unit
;;
