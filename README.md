# gcaltools
Simple functions to download Google Calendar data. 

## Setup

1. Go to https://console.developers.google.com/apis
2. Click on `Library` on the left menu
3. Then search for `Google Calendar API`
4. Turn on the API by going through `Enable API` (You may have to create a new project using the top bar on this page, after that you should be able to Enable the API).
5. Then click on `Create Credentials` > Select `Google Calendar API` or `OAuth client ID` > Choose `Other Non-UI` or `Other` for where you call API from 
6. After that you should be able to get to a point where you get two items, (1) `Client ID` and (2) `Client Secret`. If it asks you for more then that, you have run into trouble and try again. 
7. Save this to your `.Renviron` file using the following format 

```

GOOGLE_CLIENT_ID = 699555748731-3r345435345345lsdhfodisfj9nsbsidtg.apps.googleusercontent.com
GOOGLE_CLIENT_SECRET = ugOr3434343sfdfdsdCPS55z7y


```
8. Restart your RStudio

## Run

9. Install and run the following: 

```
devtools::install_github('emadretina/gcaltools')
calendar_data_df <- gcaltools::get_google_calendar_df()
```



