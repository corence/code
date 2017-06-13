{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LearnositySdk
--( convertRequestPacketToString
( insertSecurityInformationToAssessObject
, generateSignature
, hashSignatureArray
, init
) where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy
import Data.Generics.Aliases
import Data.Hashable
import Data.Time
import GHC.Generics
import qualified Data.HashMap.Strict as HM

type SecurityPacket = Value
type Secret = Value
type QuestionsApiActivity = Value
type RequestString = Value
type Action = Value
type Signature = Value
type Signatures = Value

copy_fields :: (Eq k, Hashable k) => HM.HashMap k v -> HM.HashMap k v -> [k] -> HM.HashMap k v
copy_fields dest _ [] = dest
copy_fields dest source (key:keys) = (copy_fields dest source keys) <-- (key, HM.lookup key source)

(-->) :: (Eq k, Hashable k) => HM.HashMap k v -> k -> Maybe v
(-->) collection key = HM.lookup key collection

(<--) :: (Eq k, Hashable k) => HM.HashMap k v -> (k, Maybe v) -> HM.HashMap k v
(<--) collection (key, value) = maybe collection (\value -> HM.insert key value collection) value

{-
/**
 * @param requestPacket
 * @returns string
 */
function convertRequestPacketToString(requestPacket) {
    if (requestPacket && typeof requestPacket !== 'string') {
        return JSON.stringify(requestPacket);
    } else {
        return requestPacket;
    }
}
-}

todoJsonDecode :: ByteString -> Value
todoJsonDecode stringy = eitherDecode stringy :: Parser Value

todoJsonEncode :: Value -> ByteString
todoJsonEncode = encode

--insert :: k -> v -> HashMap k v -> HashMap k v
--Data.HashMap.Strict.lookup :: k -> HashMap k v -> Maybe v

insertSecurityInformationToAssessObject :: SecurityPacket -> Secret -> QuestionsApiActivity -> QuestionsApiActivity
insertSecurityInformationToAssessObject securityPacket secret questionsActivity 
  = questionsActivity <-- ("consumer_key", securityPacket --> "consumer_key")
                      <-- ("timestamp", securityPacket --> "timestamp")
                      <-- ("user_id", securityPacket --> "user_id")
                      <-- ("signature", signature)
    where domain = domain securityPacket `orElse` domain questionsActivity `orElse` "assess.learnosity.com"
          --activity2 = copy_fields questionsActivity securityPacket ["consumer_key", "timestamp", "user_id", "signature"]
          signature = hashSignatureArray [consumer_key, domain, timestamp, user_id, secret]
          consumer_key = securityPacket --> "consumer_key"
          timestamp = securityPacket --> "timestamp"
          user_id = securityPacket --> "user_id"

{-
/**
 * Creates the signature hash.
 *
 * @param service        string
 * @param securityPacket object
 * @param secret         string
 * @param requestString  string
 * @param action         object
 */
function generateSignature(
    service,
    securityPacket,
    secret,
    requestString,
    action
) {}
-}
generateSignature :: Service -> SecurityPacket -> Secret -> RequestString -> Action -> ()
generateSignature = undefined

{-
/**
 * Joins an array (with '_') and hashes it.
 *
 * @param signatureArray array
 * @returns string
 */
function hashSignatureArray(signatureArray) {
    return sha256(signatureArray.join('_')).toString();
}
-}
hashSignatureArray :: [Signature] -> Signatures
hashSignatureArray = undefined

type RequestPacket = String -- this is a json string

data Service = Questions | Assess | Items

init :: Service -> SecurityPacket -> Secret -> RequestPacket -> Action -> ()
init service securityPacket secret requestPacket action = do
    -- if the security packet has no timestamp, then we generate it
    timestamp <- fmap (formatTime defaultTimeLocale "%Y%M%D-%H%m") getCurrentTime
    --if service == Assess
    pure ()

{-
    if (service === 'assess') {
        insertSecurityInformationToAssessObject(requestPacket, securityPacket, secret);
    }

    // Automatically populate the user_id of the security packet.
    if (_.contains(['author', 'data', 'items', 'reports'], service)) {
        // The Events API requires a user_id, so we make sure it's a part
        // of the security packet as we share the signature in some cases
        if (!securityPacket.user_id && requestPacket && requestPacket.user_id) {
            securityPacket.user_id = requestPacket.user_id;
        }
    }

    // Generate the signature based on the arguments provided
    securityPacket.signature = generateSignature(
        service,
        securityPacket,
        secret,
        requestString,
        action
    );

    var output;
    if(service === 'data') {
         output = {
            'security': JSON.stringify(securityPacket),
            'request': JSON.stringify(requestPacket),
            'action': action
        };
    } else if (service === 'questions') {
        output = _.extend(securityPacket, requestPacket);
    } else if (service === 'assess') {
        output = requestPacket;
    } else {
        output = {
            'security': securityPacket,
            'request': requestPacket
        };
    }

    return output;
-}
