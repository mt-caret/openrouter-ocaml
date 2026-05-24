open! Core
open! Async
open Openrouter_api
open Fixture_helpers

(* /models is large (hundreds of entries × deeply-nested per-model records),
   so instead of snapshotting every model we verify the full body parses
   without error and snapshot a summary: the envelope ([total_count] /
   [links]), the first model in the list, and one feature-rich model that
   populates [Reasoning], [Benchmarks], and [Pricing.Override]. *)
let%expect_test "models_list" =
  let%bind body = Reader.file_contents "responses/models_list.json" in
  let response =
    body |> Jsonaf.parse |> Or_error.ok_exn |> Models.For_testing.response_of_jsonaf
  in
  print_s
    [%message
      ""
        ~count:(List.length response.data : int)
        ~total_count:(response.total_count : int option)
        ~links:(response.links : Models.Response.Links.t option)
        ~first:(List.hd_exn response.data : Models.Model_info.t)
        ~rich:
          (List.find_exn response.data ~f:(fun model ->
             String.equal model.id "x-ai/grok-4.5")
           : Models.Model_info.t)];
  let%bind () = flush_log () in
  [%expect
    {|
    ((count 344) (total_count (344)) (links (((next ()))))
     (first
      ((id thinkingmachines/inkling)
       (canonical_slug thinkingmachines/inkling-20260715)
       (hugging_face_id (thinkingmachines/Inkling))
       (name "Thinking Machines: Inkling") (created 1784325956)
       (description
        ("Inkling is an open-weight multimodal mixture-of-experts model from Thinking Machines Lab, with 41B active parameters out of 975B total. It is designed for general-purpose reasoning, coding, agentic and tool-use systems,..."))
       (context_length (1048576))
       (pricing
        ((prompt 1E-06) (completion 4.05E-06) (request ()) (image ())
         (image_token ()) (image_output ()) (audio ()) (audio_output ())
         (input_audio_cache ()) (web_search ()) (internal_reasoning ())
         (input_cache_read (1.7E-07)) (input_cache_write ())
         (input_cache_write_1h ()) (discount ()) (overrides ())))
       (architecture
        ((tokenizer (Other)) (instruct_type ())
         (modality (text+image+audio->text))
         (input_modalities (text image audio)) (output_modalities (text))))
       (top_provider
        ((context_length (524288)) (max_completion_tokens ())
         (is_moderated false)))
       (per_request_limits ())
       (supported_parameters
        (frequency_penalty include_reasoning logit_bias max_tokens min_p
         presence_penalty reasoning reasoning_effort repetition_penalty stop
         temperature tool_choice tools top_k top_p))
       (default_parameters
        (((temperature ()) (top_p ()) (top_k ()) (frequency_penalty ())
          (presence_penalty ()) (repetition_penalty ()))))
       (supported_voices ()) (knowledge_cutoff ()) (expiration_date ())
       (links
        (((details (/api/v1/models/thinkingmachines/inkling-20260715/endpoints)))))
       (benchmarks
        (((artificial_analysis
           (((intelligence_index (40.7)) (coding_index (52.1))
             (agentic_index (32.3)))))
          (design_arena ()))))
       (reasoning
        (((mandatory false) (default_enabled (true)) (default_effort (high))
          (supported_efforts (max high medium low minimal none))
          (supports_max_tokens ()))))))
     (rich
      ((id x-ai/grok-4.5) (canonical_slug x-ai/grok-4.5-20260708)
       (hugging_face_id ()) (name "xAI: Grok 4.5") (created 1783523154)
       (description
        ("Grok 4.5 is SpaceXAI's smartest model with frontier performance on coding, knowledge work, and STEM."))
       (context_length (500000))
       (pricing
        ((prompt 2E-06) (completion 6E-06) (request ()) (image ())
         (image_token ()) (image_output ()) (audio ()) (audio_output ())
         (input_audio_cache ()) (web_search (0.005)) (internal_reasoning ())
         (input_cache_read (3E-07)) (input_cache_write ())
         (input_cache_write_1h ()) (discount ())
         (overrides
          (((min_prompt_tokens 200000) (prompt 4E-06) (completion 1.2E-05)
            (audio ()) (input_audio_cache ()) (input_cache_read (6E-07))
            (input_cache_write ()) (input_cache_write_1h ()))))))
       (architecture
        ((tokenizer (Grok)) (instruct_type ()) (modality (text+image+file->text))
         (input_modalities (text image file)) (output_modalities (text))))
       (top_provider
        ((context_length (500000)) (max_completion_tokens ())
         (is_moderated false)))
       (per_request_limits ())
       (supported_parameters
        (frequency_penalty include_reasoning logprobs max_tokens presence_penalty
         reasoning reasoning_effort response_format seed stop structured_outputs
         temperature tool_choice tools top_logprobs top_p))
       (default_parameters
        (((temperature ()) (top_p ()) (top_k ()) (frequency_penalty ())
          (presence_penalty ()) (repetition_penalty ()))))
       (supported_voices ()) (knowledge_cutoff ()) (expiration_date ())
       (links (((details (/api/v1/models/x-ai/grok-4.5-20260708/endpoints)))))
       (benchmarks
        (((artificial_analysis
           (((intelligence_index (53.8)) (coding_index (72.4))
             (agentic_index (45.7)))))
          (design_arena
           (((arena agents) (category agenticgamedev) (elo 1235) (rank 5)
             (win_rate 52.4))
            ((arena agents) (category androidnative) (elo 1227) (rank 12)
             (win_rate 53.4))
            ((arena agents) (category fullstack) (elo 1302) (rank 4)
             (win_rate 60.4))
            ((arena agents) (category godotgamedev) (elo 1302) (rank 2)
             (win_rate 60.6))
            ((arena agents) (category htmlslides) (elo 1241) (rank 2)
             (win_rate 55.3))
            ((arena agents) (category python-pptxslides) (elo 1239) (rank 7)
             (win_rate 50.7))
            ((arena agents) (category webapps) (elo 1271) (rank 6)
             (win_rate 52.7))
            ((arena models) (category 3d) (elo 1346) (rank 4) (win_rate 61.3))
            ((arena models) (category codecategories) (elo 1328) (rank 4)
             (win_rate 58.2))
            ((arena models) (category dataviz) (elo 1313) (rank 6)
             (win_rate 58.5))
            ((arena models) (category gamedev) (elo 1330) (rank 8)
             (win_rate 54.4))
            ((arena models) (category uicomponent) (elo 1339) (rank 5)
             (win_rate 56.5))
            ((arena models) (category website) (elo 1326) (rank 5)
             (win_rate 58.3)))))))
       (reasoning
        (((mandatory true) (default_enabled (true)) (default_effort (high))
          (supported_efforts (high medium low)) (supports_max_tokens ())))))))
    |}];
  Deferred.unit
;;
