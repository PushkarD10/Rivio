const JatriConfig = require("./../../src/MerchantConfigs/JatriSaathiConfig.js");
const NammaYatriConfig = require("./../../src/MerchantConfigs/NammaYatriConfig.js");
const YatriConfig = require("./../../src/MerchantConfigs/YatriConfig.js");
const UnknownMerchant = require("./../../src/MerchantConfigs/UnknownMerchant.js");
const HindiStrings = require("./../../src/Strings/HI.js");
const KannadaStrings = require("./../../src/Strings/KN.js");
const EnglishStrings = require("./../../src/Strings/EN.js");
const BengaliStrings = require("./../../src/Strings/BN.js");
const MalayalamStrings = require("./../../src/Strings/ML.js");


export const getStringFromConfig = function (key) {
  switch (window.merchantID) {
    case "JATRISAATHI":
      if (JatriConfig.isMerchantString(key)) {
        return JatriConfig.getMerchantString(key);
      }
      break;
    case "NAMMAYATRI":
      if (NammaYatriConfig.isMerchantString(key)) {
        return NammaYatriConfig.getMerchantString(key);
      }
      break;
    case "YATRI":
      if (YatriConfig.isMerchantString(key)) {
        return YatriConfig.getMerchantString(key);
      }
      break;
    case "UNKNOWN":
        if (UnknownMerchant.isMerchantString(key)) {
          return UnknownMerchant.getMerchantString(key);
        }
    default:
      return getStringFromCommon(key);
  }
  return getStringFromCommon(key);
}

export const getValueFromConfig = function (constructorKey){
  let key = constructorKey.trim(); 
  switch(window.merchantID) {
      case "JATRISAATHI" :
          return JatriConfig.getMerchantConfig(key);
      case "NAMMAYATRI" :
          return NammaYatriConfig.getMerchantConfig(key);
      case "YATRI" :
          return YatriConfig.getMerchantConfig(key);
      case "UNKNOWN" :
          return UnknownMerchant.getMerchantConfig(key);
      default:
          console.error("no value found for key "+ key);
          return "";
    }
}

function getStringFromCommon(key) {
  var selectedLanguage = JBridge.getKeysInSharedPref("LANGUAGE_KEY");
  switch (selectedLanguage) {
    case "HI_IN":
      return HindiStrings.getStringValue(key);
    case "KN_IN":
      return KannadaStrings.getStringValue(key);
    case "BN_IN":
      return BengaliStrings.getStringValue(key);
    case "ML_IN":
      return MalayalamStrings.getStringValue(key);
    default:
      return EnglishStrings.getStringValue(key);
  }
}

export const getENStrings = function (constructorKey){
  return EnglishStrings.getStringValue(constructorKey);
}

export const getMerchantId = function(id) {
  return window.merchantID;
}