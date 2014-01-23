{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS -fwarn-unused-imports -fwarn-incomplete-patterns #-}

{-| Some combinators for writing 'Arbitrary' instances.  #-}
module Test.WebApp.Arbitrary where

import Control.Applicative
import Control.Monad hiding (mapM, forM)
import Data.Function
import Data.Hashable
import Data.HashMap.Strict (HashMap (..))
-- import Data.Traversable (mapM)
import Data.Int
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.String.Conversions
import Data.Traversable
import Prelude hiding (mapM)
import Test.QuickCheck as Q

-- import qualified Data.Aeson as JS
import qualified Data.Attoparsec.Number
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map hiding (Map)
import qualified Data.Vector as V



-- * combinators

someOfMap :: (Eq k, Hashable k) => HashMap k a -> Gen (HashMap k a)
someOfMap m = (\ ks -> HashMap.filterWithKey (\ k _ -> k `elem` ks) m) <$> someOfLinear (HashMap.keys m)

-- | (Use 'someOfLinear' instead?)
someOfBellCurve :: [a] -> Gen [a]
someOfBellCurve = fmap catMaybes . mapM (\ x -> frequency [(1, pure $ Just x), (1, pure Nothing)])

-- | I think this is more interesting than 'someOfBellCurve', because
-- the fringe cases are more likely.
someOfLinear :: [a] -> Gen [a]
someOfLinear l = choose (0, length l) >>= \ l' -> take l' <$> permute l

permute :: [a] -> Gen [a]
permute [] = pure []
permute [x] = pure [x]
permute l@(_:_) = choose (0, length l - 1) >>= \ i ->
                    let (ls, r:rs) = splitAt i l in (r:) <$> permute (ls ++ rs)

test_permute :: IO ()
test_permute = do
  xs <- map (!!0) . head <$> sample' (vectorOf 30000 (permute [1..19]))
  let stats :: [(Int, Int)] = Map.toList $ foldl (\ m i -> Map.alter (Just . maybe 1 (+1)) i m) Map.empty xs
  mapM_ print stats

  -- (this does not test if every element of the input list appears in
  -- every position of the output list with equal probability.)

arbitrary' :: Arbitrary a => Gen a
arbitrary' = sized ((`resize` arbitrary) . (`div` 6))



-- * specialized arbitraries

-- | An arbitrary selection of commonly used surnames in germany.
readableStringCollection :: [ST]
readableStringCollection = ["Fern green","French blue","Carmine red","Orange","Sienna","Portland Orange","Cocoa brown","Light carmine pink","Pastel brown","Cornell Red","Spring bud","Screamin Green","Maize","Pale red violet","Medium turquoise","Cream","Lavender pink","Ultramarine blue","French beige","Vivid burgundy","White smoke","Cal Poly Pomona green","Hot pink","Deep cerise","Stizza","Vanilla","Teal","Light taupe","Bulgarian rose","Satin sheen gold","Deep peach","Viridian","Rich carmine","Bazaar","Sapphire","Patriarch","Tiffany Blue","Rose gold","Orange Yellow","White","Lavender indigo","Thulian pink","Fire engine red","Boston University Red","Coquelicot","Ochre","Thistle","Cambridge Blue","Pastel green","Cinereous","Wheat","Laser Lemon","Byzantium","Ash grey","Copper rose","Sepia","Asparagus","Carrot orange","Egyptian blue","Dark raspberry","Chartreuse","Electric crimson","Vivid cerise","Fluorescent yellow","Ultra pink","Myrtle","Cornflower blue","Dark sea green","Cinnabar","Violet Red","Urobilin","Rose taupe","Blue Bell","Bright turquoise","Light yellow","Yellow Orange","Lust","Iris","Brink pink","Bright pink","Red brown","Rose ebony","Turquoise","Ruby","Sunset Orange","Grullo","Apricot","Tomato","Tuscan red","Lavender blush","Green","Cyan","Electric lavender","Light brown","Misty rose","Harvest Gold","Majorelle Blue","Blue purple","Zaffre","Old gold","Olive Green","UA blue","Deep carmine pink","Fallow","Tickle Me Pink","Raspberry","Cadet blue","Pale plum","Azure","Carnelian","Slate blue","Carolina blue","Bleu de France","Prussian blue","Lemon chiffon","Bisque","Cardinal","University of California Gold","Crimson Red","Famous","Rich lilac","Lemon Yellow","Feldgrau","Isabelline","Psychedelic purple","Fern","Army green","Dark pink","Lime","Battleship grey","Laurel green","Sap green","Fandango","Lilac","Pistachio","Blush","Pale pink","Lawn green","Bittersweet","Harlequin","Persian rose","Napier green","Munsell","Almond","Atomic tangerine","Rosy brown","Pastel yellow","Smoky black","Taupe","Hunter green","Pink pearl","Lavender magenta","Cornsilk","Pale carmine","Gray","Celadon","North Texas Green","Palatinate blue","Brown","Electric cyan","Caribbean green","Cadmium green","Dark taupe","Cool black","British racing green","Ecru","Navajo white","June bud","Warm black","Dark slate gray","Light salmon","Deep chestnut","Lavender gray","Raspberry rose","Verdigris","Flame","Dark pastel purple","Pale silver","Sinopia","Dark pastel green","Cadet","Pansy purple","Dark coral","Light goldenrod yellow","Mountain Meadow","Light apricot","Dogwood rose","Emerald","Salmon pink","French lilac","Indian yellow","Cerise pink","Citrine","Tangelo","Vegas gold","Nadeshiko pink","Pearl Aqua","Deep champagne","Carnation pink","Jasper","Lavender mist","Light salmon pink","Camouflage green","Neon Carrot","Persian pink","Bubble gum","Tawny","Glitter","Maroon","Red Orange","Dark orchid","Medium lavender magenta","Celeste","Pastel magenta","Dim gray","Lavender purple","Light cornflower blue","Granny Smith Apple","Dark powder blue","Puce","Pale brown","Flax","Princeton orange","Wine","Green yellow","Pale magenta","Sacramento State green","Boysenberry","Medium candy apple red","Purple","Skobeloff","Deep carrot orange","Jazzberry jam","Pink Flamingo","Raspberry glace","Peach yellow","Dark orange","Gainsboro","Manatee","Teal green","Hooker green","Lemon","Magnolia","Trolley Grey","Dark olive green","Light sea green","Pastel purple","Payne grey","Pearl","Dark terra cotta","Persian red","Mulberry","Hansa yellow","Camel","Blue","Veronica","Cadmium orange","Upsdell red","Light pastel purple","Meat brown","Mountbatten pink","Light blue","Magenta","Electric yellow","Desert sand","Shocking pink","Bottle green","Mauve taupe","Terra cotta","Carmine pink","Dark blue","Bondi blue","CG Blue","Lavender blue","Bronze","Folly","Rich black","True Blue","Medium slate blue","Pink Sherbet","Bright lavender","Harvard crimson","Saint Patrick Blue","Hot magenta","Golden poppy","Deep magenta","Jade","Celestial blue","Purple taupe","Halay\224 \250be","Macaroni and Cheese","Radical Red","Turquoise blue","Royal blue","Pear","Tractor red","Tangerine","Banana yellow","Mikado yellow","Coral","Light gray","Eton blue","Shamrock green","Russet","Deep carmine","Pale spring bud","Brandeis blue","Turquoise green","Vermilion","Copper","Mordant red 19","Sky magenta","Black","Capri","CG Red","Non-photo blue","Wild Strawberry","Pale gold","Cosmic latte","Wenge","Periwinkle","Field drab","Bistre","Fuchsia pink","Carmine","Light Thulian pink","Rose","Light coral","La Salle Green","Mantis","Teal blue","Maya blue","Medium carmine","Purple Heart","Dark lava","Rosso corsa","Baby blue","Dark red","Sea blue","Paris Green","Tyrian purple","Dark cyan","Light fuchsia pink","Mint green","Deep fuchsia","Canary","Blond","Robin's Egg Blue","Silver","Caf\233 au lait","Medium aquamarine","Sea green","Cadmium red","Lavender rose","Tan","Dartmouth green","Orchid","Zinnwaldite brown","Medium Persian blue","Pastel red","Gray asparagus","Orange red","Caput mortuum","Ocean Boat Blue","Goldenrod","Han purple","Tufts Blue","Daffodil","Antique brass","Brilliant lavender","Columbia blue","Pale aqua","Tea rose","Wild blue yonder","Cherry","Wisteria","Byzantine","Moccasin","Umber","Deep pink","Electric purple","Earth yellow","Smalt","Persian plum","Tangerine yellow","American rose","Blue violet","Royal purple","Awesome","Razzle dazzle rose","Burnt umber","Blizzard Blue","Deep sky blue","Jungle green","Pale violet red","Coral red","Ghost white","Shadow","Malachite","Ball Blue","Beau blue","UP Maroon","Vivid tangerine","Sunglow","Mango Tango","India green","Fluorescent orange","Rose madder","Dark magenta","Light Crimson","Timberwolf","Honeydew","Dark jungle green","United Nations blue","Dark khaki","Shamrock","Sunset","Seashell","Dark lavender","Rifle green","Navy blue","Razzmatazz","Dark green","Pumpkin","Dandelion","Vivid auburn","Purple pizzazz","Hollywood cerise","Heart Gold","Golden brown","Palatinate purple","Corn","Pastel gray","Deep jungle green","Bright cerulean","Slate gray","Cornflower","Spring green","Vivid violet","Cotton candy","Peridot","Cherry blossom pink","Yellow green","Medium spring bud","Ruddy brown","Rose vale","Dark violet","Blue Gray","Liver","Pacific Blue","Midnight blue","Ultramarine","Otter brown","Firebrick","Persian blue","Dark gray","Pale green","Eggshell","Pale cornflower blue","Dark cerulean","Dodger blue","Pale chestnut","Gold","Ruddy","Crimson","Forest green","Han blue","Dark tangerine","Lincoln green","Medium electric blue","Rose bonbon","Falu red","Rich maroon","Papaya whip","Phthalo blue","Stormcloud","Stil de grain yellow","Aquamarine","Burnt sienna","Purple Mountain's Majesty","Yellow","Classic rose","Pale cerulean","Deep saffron","Rosewood","Plum","Brilliant rose","AuroMetalSaurus","UP Forest green","Light green","Xanadu","Aqua","Peach puff","Neon green","UCLA Gold","Ferrari Red","Red violet","Raspberry pink","Dark sienna","Naples yellow","Titanium yellow","Ube","Rich electric blue","Medium purple","Caf\233 noir","Rust","Electric green","Dark electric blue","Orange peel","Olivine","Davy grey","Moss green","Dollar bill","Chestnut","Denim","Outer Space","Chamoisee","Arylide yellow","Oxford Blue","Violet Blue","Dark pastel red","Medium spring green","Flamingo pink","Unmellow Yellow","Pale copper","French rose","Purple mountain majesty","Dark candy apple red","Pale goldenrod","Dark slate blue","Medium red violet","Old rose","Bright maroon","Dark scarlet","Pakistan green","Magic mint","Cerulean blue","Cinnamon","Royal azure","Aureolin","KU Crimson","Alice blue","Linen","Gamboge","Baby pink","Old lavender","Anti-flash white","Mode beige","Neon fuchsia","Saffron","Medium blue","Medium jungle green","Rose quartz","Dark goldenrod","Phthalo green","Bubbles","Taupe gray","MSU Green","Tea green","Seal brown","Bright green","Light slate gray","Phlox","Indigo","USC Cardinal","Wild Watermelon","Cerise","Pale robin egg blue","Olive Drab","Medium taupe","Candy apple red","Scarlet","Pastel violet","Bright ube","Peach","Lapis lazuli","Auburn","Lime green","International orange","Midnight green","Pale blue","Persian indigo","Baby blue eyes","Mint cream","Yale Blue","USC Gold","Fuzzy Wuzzy","Beige","Golden yellow","Lemon lime","Air Force blue","Cobalt","Beaver","Moonstone blue","Sand","Mahogany","Medium sea green","Languid lavender","Candy pink","Mustard","Chrome yellow","Piggy pink","Ruddy pink","Chocolate","Rackley","Desert","Electric violet","Brick red","Snow","Islamic green","Venetian red","Pine green","Royal fuchsia","Electric blue","Bole","Old mauve","Blue green","Outrageous Orange","Debian red","Green Blue","Coffee","Medium orchid","Twilight lavender","Dark chestnut","Safety orange","Onyx","Antique fuchsia","Apple green","Pastel blue","Fluorescent pink","Office green","Alizarin crimson","Pink","Inchworm","Sandy brown","Sandstorm","Fulvous","Amber","Duke blue","Fawn","Crimson glory","Charcoal","Sand dune","Dark spring green","Mint","Steel blue","International Klein Blue","Light khaki","Medium teal blue","Fashion fuchsia","Pastel pink","Dark tan","Tiger eye","Olive","Dark midnight blue","Pastel orange","Khaki","Medium champagne","Salmon","Mauvelous","Dark brown","Brass","Android Green","Fuchsia","Sky blue","Eggplant","Straw","Raw Sienna","Azure mist/web","Lion","Saddle brown","Turkish rose","Persian orange","Cadmium yellow","Cadet grey","Ao","Antique white","Glaucous","Melon","Selective yellow","Burgundy","Electric indigo","Old lace","Violet","Electric ultramarine","Amethyst","Drab","Lavender","Amaranth","Dark byzantium","Rose pink","Ginger","Red","Deep coffee","Indian red","UCLA Blue","Bone","Canary yellow","Tropical rain forest","Flavescent","Jonquil","Dark pastel blue","Pale taupe","Mauve","Electric lime","UA red","Cerulean","Burlywood","Dark salmon","Coral pink","Champagne","Burnt orange","Cordovan","Icterine","Kelly green","Utah Crimson","Floral white","Heliotrope","Light cyan","Deep lilac","Platinum","Ivory","Opera mauve","Smokey topaz","Tumbleweed","Topaz","Toolbox","Banana Mania","Spiro Disco Ball","Powder blue","UFO Green","Pale lavender","Lava","Jasmine","Blanched Almond","Buff","Medium violet red","Dark turquoise","Waterspout","Rufous","Sandy taupe","Cool grey","Light pink","Light sky blue","School bus yellow","Guppie green"]


-- | not sure if this works, or if it can work, but the idea is that
-- numbers generated by this function do not have rounding errors.
-- Also the return type is what aeson expects.
arbitraryNumber :: Double -> Gen Data.Attoparsec.Number.Number
arbitraryNumber bound = attempt2
  where
    -- not good!
    attempt1 = fromRational . (/ toRational bound) . toRational . (round :: Double -> Integer) . (* bound) <$> arbitrary

    -- good?
    attempt2 = fromRational . toRational . (`mod` round bound) <$> (arbitrary :: Gen Int)



-- * fuzzing

-- | Introduce small differences that are probably errors.  Instead of
-- this, we may want to use previous work that got this right [1][2].
--
-- [1] https://www.ee.oulu.fi/research/ouspg/Radamsa
-- [2] http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.127.2628&rep=rep1&type=pdf
class Fuzz a where
  fuzz :: a -> Gen a

instance Fuzz LBS where
  fuzz s = oneof [add_, change_, drop_]
    where
      add_ :: Gen LBS
      add_ = do
         i <- choose (0, LBS.length s)
         c <- arbitrary
         case LBS.splitAt i s of
            (a, b) -> return $ a <> LBS.pack [c] <> b

      change_ :: Gen LBS
      change_ = do
         i <- choose (0, LBS.length s)
         c <- arbitrary
         case LBS.splitAt i s of
            (a, b) -> return $ a <> LBS.pack [c] <> LBS.tail b

      drop_ :: Gen LBS
      drop_ = do
         i <- choose (0, LBS.length s - 1)
         case LBS.splitAt i s of
            (a, b) -> return $ a <> LBS.tail b

instance Fuzz SBS where
  fuzz = fmap cs . fuzz . (cs :: SBS -> LBS)

instance Fuzz ST where
  fuzz = fmap cs . fuzz . (cs :: ST -> LBS)

instance Fuzz a => Fuzz (Maybe a) where
  fuzz Nothing = return Nothing
  fuzz (Just a) = Just <$> fuzz a

instance (Fuzz a, Fuzz b, Ord a) => Fuzz (Map a b) where
  fuzz m = oneof [fuzzkey, fuzzvalue, fuzzkey >> fuzzvalue]
    where
      fuzzvalue = do
          (k, v) <- elements $ Map.assocs m
          v' <- fuzz v
          return . Map.insert k v' $ m

      fuzzkey = do
          (k, v) <- elements $ Map.assocs m
          k' <- fuzz k
          return . Map.delete k . Map.insert k' v $ m

instance (Fuzz a, Fuzz b, Eq a, Hashable a) => Fuzz (HashMap a b) where
  fuzz m = oneof [fuzzkey, fuzzvalue, fuzzkey >> fuzzvalue]
    where
      fuzzvalue = do
          (k, v) <- elements $ HashMap.toList m
          v' <- fuzz v
          return . HashMap.insert k v' $ m

      fuzzkey = do
          (k, v) <- elements $ HashMap.toList m
          k' <- fuzz k
          return . HashMap.delete k . HashMap.insert k' v $ m

instance (Fuzz a) => Fuzz (V.Vector a) where
  fuzz = return

instance Fuzz Bool where
  fuzz b = frequency [(14, pure b), (3, pure $ not b)]
