
export function currentMerchantString(key){
    var selectedLanguage = JBridge.getKeysInSharedPrefs("LANGUAGE_KEY");
    switch(selectedLanguage) {
        case "ML_IN" :
            return getStringMLValue(key);
        default:
            return getStringENValue(key);
      }
}

export function isCurrentMerchantString(key){
    return ((key in malayalamStrings) && (key in englishStrings));
}

export function getMerchantConfig(key){
    if (key in yatriPartnerConfigs){
        return yatriPartnerConfigs[key];
    }
    console.error("no value found for key "+ key);
    return "";
}

export function getStringENValue(key){
    if (key in englishStrings){
        return englishStrings[key];
    }
    console.error("no value found for key "+ key);
    return "";
}


export function getStringMLValue(key){
    if (key in malayalamStrings){
        return malayalamStrings[key];
    }
    console.error("no value found for key "+ key);
    return "";
}

const yatriPartnerConfigs = {
    RC_VALIDATION_TEXT : "KL",
    DOCUMENT_LINK : "https://docs.google.com/document/d/1zmQWO_L4EjyCXC3xSlp1f3DS2wI4HfbHxg42tXelWe0",
    APP_LINK : "https://play.google.com/store/apps/details?id=net.openkochi.yatripartner",
    PRIVACY_POLICY_LINK : "https://docs.google.com/document/d/1gI_P4oZnVwE0O71rI4Mi8rpZbL9rsIRkyewbql85Np8"
}

const malayalamStrings = {
    WELCOME_TEXT : "Welcome to the Yatri Partner",
    ABOUT_TEXT : "Yatri partner ഡ്രൈവർമാരെ റൈഡറുകളുമായി ബന്ധിപ്പിക്കുന്നതിനുള്ള ഒരു തുറന്ന പ്ലാറ്റ്ഫോമാണ്. നിർദ്ദേശിച്ച ആവശ്യമുള്ള നിരക്കുകളുള്ള റൈഡർമാരെ കണ്ടെത്താൻ ആപ്പ് ഡ്രൈവർമാർക്ക് സൗകര്യപ്രദമാക്കുന്നു. റൈഡ് അടിസ്ഥാനമാക്കിയുള്ള കമ്മീഷനില്ല, പ്രതിമാസ സബ്‌സ്‌ക്രിപ്‌ഷന്റെ രൂപത്തിൽ ചെറിയ തുക അടച്ചാൽ മതി",
    NEED_IT_TO_ENABLE_LOCATION : "ആപ്പ് അടച്ചിരിക്കുമ്പോഴും ഉപയോഗത്തിലില്ലെങ്കിലും ഡ്രൈവർ നിലവിലെ ലൊക്കേഷൻ നിരീക്ഷിക്കാൻ നിങ്ങളുടെ ലൊക്കേഷൻ പങ്കിടുന്നത് പ്രവർത്തനക്ഷമമാക്കാൻ യാത്രി പങ്കാളി ലൊക്കേഷൻ ഡാറ്റ ശേഖരിക്കുന്നു.",
    CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER : "നിലവിൽ കേരളത്തിൽ രജിസ്റ്റർ ചെയ്ത നമ്പർ മാത്രമേ ഞങ്ങൾ അനുവദിക്കൂ",
    YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT : "നിങ്ങൾ യാത്രി സപ്പോർട്ട് ടീമിലേക്ക് ഒരു കോൾ ചെയ്യാൻ പോകുകയാണ്. നിങ്ങൾക്ക് തുടരണോ?"
}

const englishStrings = {
    WELCOME_TEXT : "Welcome to the Yatri Partner",
    ABOUT_TEXT : "Yatri partner is an open platform to connect drivers with riders. The app makes it convenient for drivers to find riders with proposed desired rates. No ride based commission, just pay small amount in the form of monthly subscription",
    NEED_IT_TO_ENABLE_LOCATION : "Yatri Partner collect location data to enable share your location to monitor driver current location, even when the app is closed or not in use.",
    CURRENTLY_WE_ALLOW_ONLY_KARNATAKA_REGISTERED_NUMBER : "Currently,We allow only Kerala registered number",
    YOU_ARE_ABOUT_TO_CALL_NAMMA_YATRI_SUPPORT : "You are about to place a call to the Yatri Support Team. Do you want to proceed?"
}