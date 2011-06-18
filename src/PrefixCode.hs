module PrefixCode where

import Bit
import Control.Monad(liftM)

toPrefixCode max n = 
    let bits = intWToBits (length max) [] n
    in toPrefixCode' (zip max bits)

toPrefixCode' [] = []

toPrefixCode' ((True,True):bs) = True : toPrefixCode' bs

toPrefixCode' ((True,False):bs) = False : map snd bs

toPrefixCode' ((False, _):bs) = toPrefixCode' bs


fromPrefixCode max = liftM (mapFst bitsToInt) . fromPrefixCode' max

fromPrefixCode' [] bs                 = Just ([], bs)

fromPrefixCode' (False:max) bs        = liftM (mapFst (False :)) $ fromPrefixCode' max bs

fromPrefixCode' (True:max) []         = Nothing

fromPrefixCode' (True:max) (True:bs)  = liftM (mapFst (True :)) $ fromPrefixCode' max bs

fromPrefixCode' (True:max) (False:bs) = liftM (mapFst (False :)) $ safeSplitAt max bs


safeSplitAt [] bs = Just ([], bs)

safeSplitAt (m:ms) [] = Nothing

safeSplitAt (m:ms) (b:bs) = liftM (mapFst (b :)) $ safeSplitAt ms bs


mapFst f (x,y) = (f x, y)
