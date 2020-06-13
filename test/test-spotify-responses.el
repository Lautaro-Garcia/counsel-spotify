;; curl -X GET "https://api.spotify.com/v1/search?q=Tuyo&type=artist&limit=1" -H "Authorization: Bearer <Auth token>"
(defvar artist-spotify-response "
{
  \"artists\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=Tuyo&type=artist&offset=0&limit=1\",
    \"items\" : [ {
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/artist/3Ujv6sa60JRiaxS8RVuNOj\"
      },
      \"followers\" : {
        \"href\" : null,
        \"total\" : 66985
      },
      \"genres\" : [ \"baile pop\", \"brazilian rock\", \"indie curitibano\", \"nova mpb\", \"rio grande do sul indie\" ],
      \"href\" : \"https://api.spotify.com/v1/artists/3Ujv6sa60JRiaxS8RVuNOj\",
      \"id\" : \"3Ujv6sa60JRiaxS8RVuNOj\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/9b8fcf3f7b0be8ccca7da41b2aa84e4bca186bc7\",
        \"width\" : 640
      }, {
        \"height\" : 320,
        \"url\" : \"https://i.scdn.co/image/7056567e67d89e2e9e74b73da1b2f4fdc2f20bff\",
        \"width\" : 320
      }, {
        \"height\" : 160,
        \"url\" : \"https://i.scdn.co/image/03ba177516107316df3f536280a021c46fa7e51d\",
        \"width\" : 160
      } ],
      \"name\" : \"Tuyo\",
      \"popularity\" : 56,
      \"type\" : \"artist\",
      \"uri\" : \"spotify:artist:3Ujv6sa60JRiaxS8RVuNOj\"
    } ],
    \"limit\" : 1,
    \"next\" : \"https://api.spotify.com/v1/search?query=Tuyo&type=artist&offset=1&limit=1\",
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 9
  }
}")

;; curl -X GET "https://api.spotify.com/v1/search?q=Hiatus+Kayiote&type=album&limit=1" -H "Authorization: Bearer <Auth token>"
(defvar album-spotify-response "
{
  \"albums\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=Hiatus+Kayiote&type=album&offset=0&limit=1\",
    \"items\" : [ {
      \"album_type\" : \"album\",
      \"artists\" : [ {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/artist/43JlwunhXm1oqdKyOa2Z9Y\"
        },
        \"href\" : \"https://api.spotify.com/v1/artists/43JlwunhXm1oqdKyOa2Z9Y\",
        \"id\" : \"43JlwunhXm1oqdKyOa2Z9Y\",
        \"name\" : \"Hiatus Kaiyote\",
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:43JlwunhXm1oqdKyOa2Z9Y\"
      } ],
      \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IN\", \"IS\", \"IT\", \"JO\", \"KW\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/album/3qzmmmRmVBiOuMvrerfW4z\"
      },
      \"href\" : \"https://api.spotify.com/v1/albums/3qzmmmRmVBiOuMvrerfW4z\",
      \"id\" : \"3qzmmmRmVBiOuMvrerfW4z\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/ab67616d0000b27365c29c4a590e9e9b03467c5a\",
        \"width\" : 640
      }, {
        \"height\" : 300,
        \"url\" : \"https://i.scdn.co/image/ab67616d00001e0265c29c4a590e9e9b03467c5a\",
        \"width\" : 300
      }, {
        \"height\" : 64,
        \"url\" : \"https://i.scdn.co/image/ab67616d0000485165c29c4a590e9e9b03467c5a\",
        \"width\" : 64
      } ],
      \"name\" : \"Choose Your Weapon\",
      \"release_date\" : \"2015-05-01\",
      \"release_date_precision\" : \"day\",
      \"total_tracks\" : 18,
      \"type\" : \"album\",
      \"uri\" : \"spotify:album:3qzmmmRmVBiOuMvrerfW4z\"
    } ],
    \"limit\" : 1,
    \"next\" : null,
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 1
  }
}
")


;; curl -X GET "https://api.spotify.com/v1/search?q=Hajanga&type=track&limit=1" -H "Authorization: Bearer <Auth token>"
(defvar track-spotify-response "
{
  \"tracks\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=Hajanga&type=track&offset=0&limit=1\",
    \"items\" : [ {
      \"album\" : {
        \"album_type\" : \"album\",
        \"artists\" : [ {
          \"external_urls\" : {
            \"spotify\" : \"https://open.spotify.com/artist/0QWrMNukfcVOmgEU0FEDyD\"
          },
          \"href\" : \"https://api.spotify.com/v1/artists/0QWrMNukfcVOmgEU0FEDyD\",
          \"id\" : \"0QWrMNukfcVOmgEU0FEDyD\",
          \"name\" : \"Jacob Collier\",
          \"type\" : \"artist\",
          \"uri\" : \"spotify:artist:0QWrMNukfcVOmgEU0FEDyD\"
        } ],
        \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IN\", \"IS\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/album/4yVaTn71IU28NA0Et4bRED\"
        },
        \"href\" : \"https://api.spotify.com/v1/albums/4yVaTn71IU28NA0Et4bRED\",
        \"id\" : \"4yVaTn71IU28NA0Et4bRED\",
        \"images\" : [ {
          \"height\" : 640,
          \"url\" : \"https://i.scdn.co/image/ab67616d0000b2736d934974896ae0a6bfd6f678\",
          \"width\" : 640
        }, {
          \"height\" : 300,
          \"url\" : \"https://i.scdn.co/image/ab67616d00001e026d934974896ae0a6bfd6f678\",
          \"width\" : 300
        }, {
          \"height\" : 64,
          \"url\" : \"https://i.scdn.co/image/ab67616d000048516d934974896ae0a6bfd6f678\",
          \"width\" : 64
        } ],
        \"name\" : \"In My Room\",
        \"release_date\" : \"2016-07-01\",
        \"release_date_precision\" : \"day\",
        \"total_tracks\" : 11,
        \"type\" : \"album\",
        \"uri\" : \"spotify:album:4yVaTn71IU28NA0Et4bRED\"
      },
      \"artists\" : [ {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/artist/0QWrMNukfcVOmgEU0FEDyD\"
        },
        \"href\" : \"https://api.spotify.com/v1/artists/0QWrMNukfcVOmgEU0FEDyD\",
        \"id\" : \"0QWrMNukfcVOmgEU0FEDyD\",
        \"name\" : \"Jacob Collier\",
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:0QWrMNukfcVOmgEU0FEDyD\"
      } ],
      \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"EG\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IN\", \"IS\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SA\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
      \"disc_number\" : 1,
      \"duration_ms\" : 362996,
      \"explicit\" : false,
      \"external_ids\" : {
        \"isrc\" : \"US23A1500090\"
      },
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/track/5ciCKiLGTIgfHKV4MxjInt\"
      },
      \"href\" : \"https://api.spotify.com/v1/tracks/5ciCKiLGTIgfHKV4MxjInt\",
      \"id\" : \"5ciCKiLGTIgfHKV4MxjInt\",
      \"is_local\" : false,
      \"name\" : \"Hajanga\",
      \"popularity\" : 39,
      \"preview_url\" : \"https://p.scdn.co/mp3-preview/e9bcf722e8f9e238324c33b41a225f429ec60e8f?cid=c4b3759f11db48f79603f1690524ce95\",
      \"track_number\" : 8,
      \"type\" : \"track\",
      \"uri\" : \"spotify:track:5ciCKiLGTIgfHKV4MxjInt\"
    } ],
    \"limit\" : 1,
    \"next\" : \"https://api.spotify.com/v1/search?query=Hajanga&type=track&offset=1&limit=1\",
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 2
  }
}
")

;; curl -X GET "https://api.spotify.com/v1/search?q=Atilla&type=playlist&limit=1" -H "Authorization: Bearer <Auth token>"
(defvar playlist-spotify-response "
{
  \"playlists\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=Atila&type=playlist&offset=0&limit=1\",
    \"items\" : [ {
      \"collaborative\" : false,
      \"description\" : \"\",
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/playlist/6ROecAB4Ksv6MJKyhFchKH\"
      },
      \"href\" : \"https://api.spotify.com/v1/playlists/6ROecAB4Ksv6MJKyhFchKH\",
      \"id\" : \"6ROecAB4Ksv6MJKyhFchKH\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://mosaic.scdn.co/640/ab67616d0000b2730ad85d77175b35681287fc85ab67616d0000b2735587df43cb8e20d8f37ee286ab67616d0000b27387d25c1784c64bb0d55f6305ab67616d0000b273a2f02d9e8c6b85de49125c86\",
        \"width\" : 640
      }, {
        \"height\" : 300,
        \"url\" : \"https://mosaic.scdn.co/300/ab67616d0000b2730ad85d77175b35681287fc85ab67616d0000b2735587df43cb8e20d8f37ee286ab67616d0000b27387d25c1784c64bb0d55f6305ab67616d0000b273a2f02d9e8c6b85de49125c86\",
        \"width\" : 300
      }, {
        \"height\" : 60,
        \"url\" : \"https://mosaic.scdn.co/60/ab67616d0000b2730ad85d77175b35681287fc85ab67616d0000b2735587df43cb8e20d8f37ee286ab67616d0000b27387d25c1784c64bb0d55f6305ab67616d0000b273a2f02d9e8c6b85de49125c86\",
        \"width\" : 60
      } ],
      \"name\" : \"Atilla\",
      \"owner\" : {
        \"display_name\" : \"antoniorusso009\",
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/user/antoniorusso009\"
        },
        \"href\" : \"https://api.spotify.com/v1/users/antoniorusso009\",
        \"id\" : \"antoniorusso009\",
        \"type\" : \"user\",
        \"uri\" : \"spotify:user:antoniorusso009\"
      },
      \"primary_color\" : null,
      \"public\" : null,
      \"snapshot_id\" : \"NDEsZjYxN2E0MzI3MzNjMzE4YzIxNTJmMjM5OTFiNjNmNDkzMDZlMzljZg==\",
      \"tracks\" : {
        \"href\" : \"https://api.spotify.com/v1/playlists/6ROecAB4Ksv6MJKyhFchKH/tracks\",
        \"total\" : 59
      },
      \"type\" : \"playlist\",
      \"uri\" : \"spotify:playlist:6ROecAB4Ksv6MJKyhFchKH\"
    } ],
    \"limit\" : 1,
    \"next\" : \"https://api.spotify.com/v1/search?query=Atila&type=playlist&offset=1&limit=1\",
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 293
  }
}
")

;; curl -X GET "https://api.spotify.com/v1/search?q=emacs&type=episode&market=US&limit=1" -H "Authorization: Bearer <Auth token>"
(defvar episode-spotify-response "
{
  \"episodes\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=emacs&type=episode&market=US&offset=0&limit=1\",
    \"items\" : [ {
      \"audio_preview_url\" : \"https://p.scdn.co/mp3-preview/62789cf9bbfb9a8909c09167edc67c0d8ef35bce\",
      \"description\" : \"Is there a way to control text in the prompt for user permissions? Have we tried Emacs? How do you write better words? When is it appropriate to add a new framework to your resume? Do you think there is a front end ceiling?\",
      \"duration_ms\" : 3533636,
      \"explicit\" : false,
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/episode/4v7TCaTDO9US5bgY24D4ks\"
      },
      \"href\" : \"https://api.spotify.com/v1/episodes/4v7TCaTDO9US5bgY24D4ks\",
      \"id\" : \"4v7TCaTDO9US5bgY24D4ks\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/197bcb89d276ff3111893a33fba0fb1b7702624a\",
        \"width\" : 640
      }, {
        \"height\" : 300,
        \"url\" : \"https://i.scdn.co/image/e1c071a596668e7e59171c6c3a557e38f4c8fe80\",
        \"width\" : 300
      }, {
        \"height\" : 64,
        \"url\" : \"https://i.scdn.co/image/0ac7e180218c4f9e88d4e3c20ed4ddb3444fbc5b\",
        \"width\" : 64
      } ],
      \"is_externally_hosted\" : false,
      \"is_playable\" : true,
      \"language\" : \"en-US\",
      \"languages\" : [ \"en-US\" ],
      \"name\" : \"414: The Front-End Ceiling, Emacs, Permissions, and Writing Better Words\",
      \"release_date\" : \"2020-05-25\",
      \"release_date_precision\" : \"day\",
      \"type\" : \"episode\",
      \"uri\" : \"spotify:episode:4v7TCaTDO9US5bgY24D4ks\"
    } ],
    \"limit\" : 1,
    \"next\" : \"https://api.spotify.com/v1/search?query=emacs&type=episode&market=US&offset=1&limit=1\",
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 40
  }
}
")

;; curl -X GET "https://api.spotify.com/v1/search?q=emacs&type=show&market=US&limit=1" -H "Authorization: Bearer <Auth token>"
(defvar show-spotify-response "
{
  \"shows\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=emacs&type=show&market=US&offset=0&limit=1\",
    \"items\" : [ {
      \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IN\", \"IS\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
      \"copyrights\" : [ ],
      \"description\" : \"Exploring the world of Emacs as a beginner, learning how to grow a wonderful tool out of raw materials and with community's help.\",
      \"explicit\" : false,
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/show/55FtiTIlDijMqPPa6rGUti\"
      },
      \"href\" : \"https://api.spotify.com/v1/shows/55FtiTIlDijMqPPa6rGUti\",
      \"id\" : \"55FtiTIlDijMqPPa6rGUti\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/93c32bbdff95a713ddf24fca71d7a4412526c1d6\",
        \"width\" : 640
      }, {
        \"height\" : 300,
        \"url\" : \"https://i.scdn.co/image/b53d0967619f05e82ca3d0988ea8304882e854cd\",
        \"width\" : 300
      }, {
        \"height\" : 64,
        \"url\" : \"https://i.scdn.co/image/d898125d39b81ac578cf13b6c2c2c5857b312f4b\",
        \"width\" : 64
      } ],
      \"is_externally_hosted\" : false,
      \"languages\" : [ \"en-US\" ],
      \"media_type\" : \"audio\",
      \"name\" : \"EmacsCast\",
      \"publisher\" : \"Rakhim Davletkaliyev\",
      \"type\" : \"show\",
      \"uri\" : \"spotify:show:55FtiTIlDijMqPPa6rGUti\"
    } ],
    \"limit\" : 1,
    \"next\" : \"https://api.spotify.com/v1/search?query=emacs&type=show&market=US&offset=1&limit=1\",
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 9
  }
}
")

;; curl -X GET "https://api.spotify.com/v1/search?q=emacs&type=show&market=US&limit=2" -H "Authorization: Bearer <Auth token>"
(defvar two-shows-spotify-response "
{
  \"shows\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=emacs&type=show&market=US&offset=0&limit=2\",
    \"items\" : [ {
      \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IN\", \"IS\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
      \"copyrights\" : [ ],
      \"description\" : \"Exploring the world of Emacs as a beginner, learning how to grow a wonderful tool out of raw materials and with community's help.\",
      \"explicit\" : false,
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/show/55FtiTIlDijMqPPa6rGUti\"
      },
      \"href\" : \"https://api.spotify.com/v1/shows/55FtiTIlDijMqPPa6rGUti\",
      \"id\" : \"55FtiTIlDijMqPPa6rGUti\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/93c32bbdff95a713ddf24fca71d7a4412526c1d6\",
        \"width\" : 640
      }, {
        \"height\" : 300,
        \"url\" : \"https://i.scdn.co/image/b53d0967619f05e82ca3d0988ea8304882e854cd\",
        \"width\" : 300
      }, {
        \"height\" : 64,
        \"url\" : \"https://i.scdn.co/image/d898125d39b81ac578cf13b6c2c2c5857b312f4b\",
        \"width\" : 64
      } ],
      \"is_externally_hosted\" : false,
      \"languages\" : [ \"en-US\" ],
      \"media_type\" : \"audio\",
      \"name\" : \"EmacsCast\",
      \"publisher\" : \"Rakhim Davletkaliyev\",
      \"type\" : \"show\",
      \"uri\" : \"spotify:show:55FtiTIlDijMqPPa6rGUti\"
    }, {
      \"available_markets\" : [ \"AD\", \"AE\", \"AR\", \"AT\", \"AU\", \"BE\", \"BG\", \"BH\", \"BO\", \"BR\", \"CA\", \"CH\", \"CL\", \"CO\", \"CR\", \"CY\", \"CZ\", \"DE\", \"DK\", \"DO\", \"DZ\", \"EC\", \"EE\", \"ES\", \"FI\", \"FR\", \"GB\", \"GR\", \"GT\", \"HK\", \"HN\", \"HU\", \"ID\", \"IE\", \"IL\", \"IN\", \"IS\", \"IT\", \"JO\", \"JP\", \"KW\", \"LB\", \"LI\", \"LT\", \"LU\", \"LV\", \"MA\", \"MC\", \"MT\", \"MX\", \"MY\", \"NI\", \"NL\", \"NO\", \"NZ\", \"OM\", \"PA\", \"PE\", \"PH\", \"PL\", \"PS\", \"PT\", \"PY\", \"QA\", \"RO\", \"SE\", \"SG\", \"SK\", \"SV\", \"TH\", \"TN\", \"TR\", \"TW\", \"US\", \"UY\", \"VN\", \"ZA\" ],
      \"copyrights\" : [ ],
      \"description\" : \"Aventuras , diversiones e información del día a día de un Acapulqueño 🌴⚡🤙\",
      \"explicit\" : false,
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/show/1M7pvExXz0Se3vmrtTHNb9\"
      },
      \"href\" : \"https://api.spotify.com/v1/shows/1M7pvExXz0Se3vmrtTHNb9\",
      \"id\" : \"1M7pvExXz0Se3vmrtTHNb9\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/46d361f54fcdc30f22d649fcc19f0bfb419b91a7\",
        \"width\" : 640
      }, {
        \"height\" : 300,
        \"url\" : \"https://i.scdn.co/image/0ffed141c19f9eb44af76af3f68863b7e66c6bca\",
        \"width\" : 300
      }, {
        \"height\" : 64,
        \"url\" : \"https://i.scdn.co/image/2294c16aaff030a8ea38e6aa79fc20759ac532b2\",
        \"width\" : 64
      } ],
      \"is_externally_hosted\" : false,
      \"languages\" : [ \"es\" ],
      \"media_type\" : \"audio\",
      \"name\" : \"Emack (El Brody ) Ramírez \",
      \"publisher\" : \"Emanuel Ramirez\",
      \"type\" : \"show\",
      \"uri\" : \"spotify:show:1M7pvExXz0Se3vmrtTHNb9\"
    } ],
    \"limit\" : 2,
    \"next\" : \"https://api.spotify.com/v1/search?query=emacs&type=show&market=US&offset=2&limit=2\",
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 9
  }
}
")

;; curl -X GET "https://api.spotify.com/v1/search?q=nakamarra&type=album,artist&market=US&limit=1" -H "Authorization: Bearer <Auth token>"
(defvar artist-and-album-spotify-response "
{
  \"albums\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=nakamarra&type=album&market=US&offset=0&limit=1\",
    \"items\" : [ {
      \"album_type\" : \"album\",
      \"artists\" : [ {
        \"external_urls\" : {
          \"spotify\" : \"https://open.spotify.com/artist/7IlRNXHjoOCgEAWN5qYksg\"
        },
        \"href\" : \"https://api.spotify.com/v1/artists/7IlRNXHjoOCgEAWN5qYksg\",
        \"id\" : \"7IlRNXHjoOCgEAWN5qYksg\",
        \"name\" : \"Aya Nakamura\",
        \"type\" : \"artist\",
        \"uri\" : \"spotify:artist:7IlRNXHjoOCgEAWN5qYksg\"
      } ],
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/album/3jqQFIXUakuDXdhFVvI7Ko\"
      },
      \"href\" : \"https://api.spotify.com/v1/albums/3jqQFIXUakuDXdhFVvI7Ko\",
      \"id\" : \"3jqQFIXUakuDXdhFVvI7Ko\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/ab67616d0000b273511e5734e15d2ead3d95129c\",
        \"width\" : 640
      }, {
        \"height\" : 300,
        \"url\" : \"https://i.scdn.co/image/ab67616d00001e02511e5734e15d2ead3d95129c\",
        \"width\" : 300
      }, {
        \"height\" : 64,
        \"url\" : \"https://i.scdn.co/image/ab67616d00004851511e5734e15d2ead3d95129c\",
        \"width\" : 64
      } ],
      \"name\" : \"NAKAMURA\",
      \"release_date\" : \"2018-11-02\",
      \"release_date_precision\" : \"day\",
      \"total_tracks\" : 13,
      \"type\" : \"album\",
      \"uri\" : \"spotify:album:3jqQFIXUakuDXdhFVvI7Ko\"
    } ],
    \"limit\" : 1,
    \"next\" : \"https://api.spotify.com/v1/search?query=nakamarra&type=album&market=US&offset=2&limit=1\",
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 11
  },
  \"artists\" : {
    \"href\" : \"https://api.spotify.com/v1/search?query=nakamarra&type=artist&market=US&offset=0&limit=1\",
    \"items\" : [ {
      \"external_urls\" : {
        \"spotify\" : \"https://open.spotify.com/artist/6dX1kzzeak2jp50G2vtKx1\"
      },
      \"followers\" : {
        \"href\" : null,
        \"total\" : 493
      },
      \"genres\" : [ \"isle of wight indie\" ],
      \"href\" : \"https://api.spotify.com/v1/artists/6dX1kzzeak2jp50G2vtKx1\",
      \"id\" : \"6dX1kzzeak2jp50G2vtKx1\",
      \"images\" : [ {
        \"height\" : 640,
        \"url\" : \"https://i.scdn.co/image/e63e0b441e73535e076d3a150a52272aa53a5512\",
        \"width\" : 640
      }, {
        \"height\" : 320,
        \"url\" : \"https://i.scdn.co/image/981d08f78e572199c13324a67b00b0324aba1e86\",
        \"width\" : 320
      }, {
        \"height\" : 160,
        \"url\" : \"https://i.scdn.co/image/97f8d76378a39350059136116954f7290e85484a\",
        \"width\" : 160
      } ],
      \"name\" : \"Nakamarra\",
      \"popularity\" : 2,
      \"type\" : \"artist\",
      \"uri\" : \"spotify:artist:6dX1kzzeak2jp50G2vtKx1\"
    } ],
    \"limit\" : 1,
    \"next\" : null,
    \"offset\" : 0,
    \"previous\" : null,
    \"total\" : 2
  }
}
")

(provide 'test-spotify-responses)
