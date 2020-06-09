{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Massalia.Auth
-- Description : A module to define utility functions for authentication using JWTs.
-- In particular, it provides utility function to check several properties of a JWT 
-- (expiration date, not before time, and signature integrity).
module Massalia.Auth
  ( module Web.JWT,
    JWTEncodedString (JWTEncodedString),
    defaultCheckJWT,
    checkJWT,
    JWTError(..)
  )
where

import Control.Monad.Except (liftEither)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time (NominalDiffTime)
import Protolude hiding (exp)
import Web.JWT

-- | A JWT claim encoded, the Token in its exchangeable format.
-- No check is performed for integrity (no smart constructor).
-- It's mostly there for documenting purposes.
newtype JWTEncodedString = JWTEncodedString Text deriving (Eq, Show, Read, Generic)

-- | All the possible errors when attempting to 'checkJWT'.
data JWTError
  = -- | The provided signature (in the JWT) and the one re-computed do not match.
    WrongSignature
  | -- | The current time value is greater than the exp value in the JWT.
    -- meaning we are __after__ the indicated expiration.
    HasExpired
  | -- | The current time is lesser than the nbt value in the JWT.
    -- meaning we are __before__ the indicated not before claim.
    NotValidYet
  | -- | The given JWT has no exp value.
    -- This error is optional, the default is an exp value is required (see CheckOptions).
    NoExpirationProvided
  | -- | The given JWT has no nbf value.
    -- This error is optional, the default is it's not required (see CheckOptions).
    NoNBFProvided
  deriving (Show, Eq)

data CheckOptions = CheckOptions
  { failOnMissingExp :: Bool,
    failOnMissingNbf :: Bool
  }
  deriving (Show)

defaultCheckOptions :: CheckOptions
defaultCheckOptions =
  CheckOptions
    { failOnMissingExp = True,
      failOnMissingNbf = False
    }

-- | A simple alias to 'checkJWT' with default options.
-- See 'checkJWT'.
defaultCheckJWT ::
  Signer ->
  JWTEncodedString ->
  ExceptT JWTError IO JWTClaimsSet
defaultCheckJWT = checkJWT Nothing

-- | A simple JWT check for all the following:
-- * signature in JWT and computed signature correspond (integrity)
-- * expiration timestamp is greater than current timestamp
-- * not before timestamp is lesser than current timestamp
-- It uses @getPOSIXTime@ for the accessing the current time.
checkJWT ::
  Maybe CheckOptions ->
  Signer ->
  JWTEncodedString ->
  ExceptT JWTError IO JWTClaimsSet
checkJWT maybeOpt secret (JWTEncodedString inputJWT) = do
  verifiedJWT <- liftEither $ maybeToRight WrongSignature verified
  let claim = claims verifiedJWT
  now <- liftIO getPOSIXTime
  validExpClaimSet <- liftEither $ checkExpiration (failOnMissingExp opt) now claim
  liftEither $ checkNotBefore (failOnMissingNbf opt) now validExpClaimSet
  where
    verified = decodeAndVerifySignature secret inputJWT
    opt = fromMaybe defaultCheckOptions maybeOpt

checkExpiration :: Bool -> NominalDiffTime -> JWTClaimsSet -> Either JWTError JWTClaimsSet
checkExpiration shouldFailOnNothing now claimSet = case exp claimSet of
  Nothing ->
    if shouldFailOnNothing
      then Left NoExpirationProvided
      else Right claimSet
  Just val ->
    if now > secondsSinceEpoch val
      then Left HasExpired
      else Right claimSet

checkNotBefore :: Bool -> NominalDiffTime -> JWTClaimsSet -> (Either JWTError JWTClaimsSet)
checkNotBefore shouldFailOnNothing now claimSet = case nbf claimSet of
  Nothing ->
    if shouldFailOnNothing
      then Left NoNBFProvided
      else Right claimSet
  Just val ->
    if now < secondsSinceEpoch val
      then Left NotValidYet
      else Right claimSet