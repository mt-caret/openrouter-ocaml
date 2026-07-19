open! Core
open! Async
open Fixture_helpers

(* Older capture whose [usage] object predates [is_byok] / [cost_details]:
   exercises the JSON-absent path for both fields. *)
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
     (usage
      ((prompt_tokens 2) (total_tokens 2) (cost (4E-08)) (is_byok ())
       (cost_details ())))
     (provider (OpenAI)) (id (gen-emb-1778404857-ROxqLrvPOiI66GL9MNIr)))
    |}];
  Deferred.unit
;;

(* Current capture: [usage] carries [is_byok] and the [cost_details]
   breakdown. *)
let%expect_test "embeddings_cost_details" =
  let%bind () = parse_embeddings_fixture "embeddings_cost_details" in
  [%expect
    {|
    ((object_ list) (model text-embedding-3-small)
     (data
      (((object_ embedding) (index 0)
        (embedding
         (-0.0704345703125 -0.40625 0.353759765625 0.298095703125 -0.25732421875
          -0.435302734375 -0.314208984375 0.51171875)))))
     (usage
      ((prompt_tokens 2) (total_tokens 2) (cost (4E-08)) (is_byok (false))
       (cost_details
        (((upstream_inference_cost (4E-08))
          (upstream_inference_prompt_cost (4E-08))
          (upstream_inference_completions_cost (0)))))))
     (provider (OpenAI)) (id (gen-emb-1784435272-xmTGij6hRKB9fRb3Zc57)))
    |}];
  Deferred.unit
;;
