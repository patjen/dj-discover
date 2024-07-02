# dj-discover
DJ Discover (University Project made in Elm)

Gain insight into your Spotify Playlists to craft your next great DJ setlist.

## How to use
Access the project directly here as a GitHub page. Due to restrictions to Spotify's Web API only whitelisted accounts can authenticate with the application. However, there are different options to access the application:

### 1. Demoing it using sample data provided
Add the _sample_data.json_ file to the applications LocalStorage calling the key 'state'. You can access your LocalStorage using DevTools under Application > LocalStorage.

### 2. Request to be whitelisted
Message me under app@patjen.de to get whitelisted

### 3. Provide your own clientID and use it locally
In **src/Main.elm** change the **clientId** value to your own provided value you can get under https://developer.spotify.com.

If you are using it locally don't forget to also update the **homeUrl** value in the elm File as well.
