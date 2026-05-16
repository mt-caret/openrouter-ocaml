open! Core
open! Async
open Fixture_helpers

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
