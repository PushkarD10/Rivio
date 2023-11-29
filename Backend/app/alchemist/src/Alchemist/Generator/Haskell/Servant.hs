module Alchemist.Generator.Haskell.Servant where

import Alchemist.DSL.Syntax.API
import Alchemist.Utils
import Data.List (intercalate)
import Data.List.Extra (snoc)
import Data.Text (Text)
import qualified Data.Text as T
import Prelude

generateServantAPI :: Apis -> String
generateServantAPI input =
  "module API.UI." <> T.unpack (_moduleName input) <> " where \n\n"
    <> intercalate "\n" (map ("import " <>) defaultImports)
    <> "\n\n"
    <> intercalate "\n" (map (makeQualifiedImport) defaultQualifiedImport)
    <> "\n\n"
    <> intercalate "\n" (makeQualifiedImport <$> figureOutImports (T.unpack <$> concatMap handlerImports (_apis input)))
    -- <> intercalate "\n"  (T.unpack <$> concatMap handlerImports (_apis input))
    <> "\nimport qualified Domain.Action.UI."
    <> T.unpack (_moduleName input)
    <> " as "
    <> "Domain.Action.UI."
    <> T.unpack (_moduleName input)
    <> T.unpack
      ( ("\n\ntype API = \n " <> T.intercalate "\n :<|> " (map apiTTToText (_apis input)))
          <> "\n \nhandler  :: Environment.FlowServer API\nhandler = "
          <> T.intercalate "\n  :<|> " (map handlerFunctionText (_apis input))
          <> "\n\n"
          <> T.intercalate "\n" (map handlerFunctionDef (_apis input))
      )
  where
    defaultImports :: [String]
    defaultImports = ["EulerHS.Prelude", "Servant", "Tools.Auth", "Kernel.Utils.Common"]

    defaultQualifiedImport :: [String]
    defaultQualifiedImport = ["Domain.Types.Person", "Domain.Types.Merchant", "Environment"]

    makeQualifiedImport :: String -> String
    makeQualifiedImport impts = "import qualified " <> impts <> " as " <> impts

    -- containsMandatoryQueryParam :: [ApiTT] -> Bool
    -- containsMandatoryQueryParam apis = any apiHasMandatoryQueryParam apis

    -- apiHasMandatoryQueryParam :: ApiTT -> Bool
    -- apiHasMandatoryQueryParam apiTT =
    --   any urlPartHasMandatoryQueryParam (_urlParts apiTT)

    -- urlPartHasMandatoryQueryParam :: UrlParts -> Bool
    -- urlPartHasMandatoryQueryParam (QueryParam _ _ isMandatory) = isMandatory
    -- urlPartHasMandatoryQueryParam _ = False

    handlerFunctionDef :: ApiTT -> Text
    handlerFunctionDef apiT =
      let functionName = handlerFunctionText apiT
          allTypes = handlerImports apiT
          showType = case filter (/= T.empty) (init allTypes) of
            [] -> T.empty
            ty -> " -> " <> T.intercalate " -> " ty
          handlerTypes = showType <> " -> Environment.FlowHandler " <> last allTypes
       in functionName <> " :: (Id Domain.Types.Person.Person.Person, Id Domain.Types.Merchant.Merchant.Merchant)" <> handlerTypes
            <> "\n"
            <> functionName
            <> " = withFlowHandlerAPI . "
            <> "Domain.Action.UI."
            <> (_moduleName input)
            <> "."
            <> functionName
            <> "\n"

apiTTToText :: ApiTT -> Text
apiTTToText apiTT =
  let urlPartsText = map urlPartToText (_urlParts apiTT)
      authTypeText = case _authType apiTT of
        Just AdminTokenAuth -> "AdminTokenAuth"
        Just TokenAuth -> "TokenAuth"
        Nothing -> ""
      apiTypeText = apiTypeToText (_apiType apiTT)
      apiReqText = apiReqToText (_apiReqType apiTT)
      apiResText = apiResToText (_apiResType apiTT)
      headerText = map headerToText (_header apiTT)
   in authTypeText <> T.concat urlPartsText <> T.concat headerText <> apiReqText <> " :> " <> apiTypeText <> apiResText
  where
    urlPartToText :: UrlParts -> Text
    urlPartToText (UnitPath path) = " :> \"" <> path <> "\""
    urlPartToText (Capture path ty) = " :> Capture \"" <> path <> "\" " <> ty
    urlPartToText (QueryParam path ty isMandatory) =
      " :> " <> (if isMandatory then "Mandatory" else "") <> "QueryParam \"" <> path <> "\" " <> ty

    apiReqToText :: Maybe ApiReq -> Text
    apiReqToText Nothing = ""
    apiReqToText (Just (ApiReq ty name)) = " :> ReqBody '[" <> ty <> "] " <> name

    apiResToText :: ApiRes -> Text
    apiResToText (ApiRes ty name) = " '[" <> ty <> "] " <> name

    headerToText :: HeaderType -> Text
    headerToText (Header name ty) = " :> Header \"" <> name <> "\" " <> ty

handlerFunctionText :: ApiTT -> Text
handlerFunctionText apiTT =
  let apiTypeText = T.toLower $ apiTypeToText (_apiType apiTT)
      urlPartsText = map urlPartToName (_urlParts apiTT)
   in apiTypeText <> T.intercalate "" (filter (/= T.empty) urlPartsText)
  where
    urlPartToName :: UrlParts -> Text
    urlPartToName (UnitPath name) = (T.toUpper . T.singleton . T.head) name <> T.tail name
    urlPartToName _ = ""

handlerImports :: ApiTT -> [Text]
handlerImports input =
  let urlTypeText = map urlToText (_urlParts input)
      headerTypeText = map (\(Header _ ty) -> ty) (_header input)
      reqTypeText = reqTypeToText $ _apiReqType input
      resTypeText = (\(ApiRes _ ty) -> ty) $ _apiResType input
   in filter (/= T.empty) (snoc (snoc (urlTypeText ++ headerTypeText) reqTypeText) resTypeText)
  where
    urlToText :: UrlParts -> Text
    urlToText (Capture _ ty) = ty
    urlToText (QueryParam _ ty _) = ty
    urlToText _ = ""

    reqTypeToText :: Maybe ApiReq -> Text
    reqTypeToText Nothing = ""
    reqTypeToText (Just (ApiReq _ ty)) = ty

apiTypeToText :: ApiType -> Text
apiTypeToText apitype = case apitype of
  GET -> "Get"
  POST -> "Post"
  PUT -> "Put"
  DELETE -> "Delete"
