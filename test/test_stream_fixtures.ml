open! Core
open! Async
open Openrouter_api
open Fixture_helpers

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

(* Streaming with [debug.echo_upstream_body = true]: OpenRouter emits a leading
   chunk with no [choices] and a top-level [debug.echo_upstream_body] object. *)
let%expect_test "stream_debug_echo_upstream_body" =
  let%bind () = parse_stream_fixture "debug_echo_upstream_body" in
  [%expect
    {|
    (((id gen-1778597035-i4alFZDcdQKCHxZwqkuH) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778597035) (choices ()) (system_fingerprint ()) (service_tier ())
      (debug
       ((Object
         ((echo_upstream_body
           (Object
            ((model (String openai/gpt-4o-mini)) (stream True)
             (stream_options (Object ((include_usage True))))
             (messages
              (Array
               ((Object
                 ((role (String user))
                  (content (String "Reply with pong only.")))))))
             (max_completion_tokens (Number 8)) (temperature (Number 1))
             (top_p (Number 1)) (frequency_penalty (Number 0))
             (presence_penalty (Number 0)) (seed Null))))))))
      (usage ()))
     ((id gen-1778597035-i4alFZDcdQKCHxZwqkuH) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778597035)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (P)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ()) (usage ()))
     ((id gen-1778597035-i4alFZDcdQKCHxZwqkuH) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778597035)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (ong)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ()) (usage ()))
     ((id gen-1778597035-i4alFZDcdQKCHxZwqkuH) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778597035)
      (choices
       (((logprobs ()) (finish_reason ()) (native_finish_reason ()) (index 0)
         (delta
          ((role (assistant)) (content (!)) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ()) (usage ()))
     ((id gen-1778597035-i4alFZDcdQKCHxZwqkuH) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778597035)
      (choices
       (((logprobs ()) (finish_reason (stop)) (native_finish_reason (stop))
         (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ()) (usage ()))
     ((id gen-1778597035-i4alFZDcdQKCHxZwqkuH) (provider Azure)
      (model openai/gpt-4o-mini) (object_ chat.completion.chunk)
      (created 1778597035)
      (choices
       (((logprobs ()) (finish_reason (stop)) (native_finish_reason (stop))
         (index 0)
         (delta
          ((role (assistant)) (content ("")) (refusal ()) (reasoning ())
           (reasoning_details ()) (images ()) (annotations ()) (tool_calls ())))
         (error ()))))
      (system_fingerprint (fp_eb37e061ec)) (service_tier ())
      (usage
       (((prompt_tokens 12) (completion_tokens 4) (total_tokens 16)
         (cost (4.2E-06)) (is_byok (false))
         (prompt_tokens_details
          (((cached_tokens (0)) (cache_write_tokens (0)) (audio_tokens (0))
            (video_tokens (0)))))
         (cost_details
          (((upstream_inference_cost (4.2E-06))
            (upstream_inference_prompt_cost (1.8E-06))
            (upstream_inference_completions_cost (2.4E-06)))))
         (completion_tokens_details
          (((reasoning_tokens (0)) (image_tokens (0)) (audio_tokens (0)))))
         (server_tool_use ()))))))
    |}];
  Deferred.unit
;;

(* Streaming audio output carries incremental [delta.audio] objects. The raw
   fixture contains base64 PCM data, so snapshot only a structural summary. *)
let%expect_test "stream_audio_output" =
  let%bind chunks = stream_fixture_chunks "stream_audio_output" in
  print_s [%sexp (audio_stream_summary chunks : Audio_stream_summary.t)];
  [%expect
    {|
    ((chunks 9) (audio_chunks 7)
     (first_audio_id (audio_6a0340e806e481919afaa2e59c6f407e))
     (transcripts (P ong !)) (data_chunks 3) (expires_at_chunks 1)
     (audio_tokens (19)))
    |}];
  Deferred.unit
;;
