# nflversepff v0.0.2

* `pff_teams()` added to pull team IDs for given season
* `pff_players()` updated to pull from team-based endpoint which supports offseason
roster updates
* `.pff_players_by_team()` low-level function added to pull player info from team
endpoint
* `.pff_players_by_position()` low-level function removed
* `options(nflversepff.verbose)` added to control verbosity

# nflversepff v0.0.1

* `pff_players()` added to pull player bio/roster information
* `scrape_players_to_release()` added to loop over pff_players and push into 
`nflverse/nflverse-data` repo.
* `.api_request()` and `.pff_request()` low-level api functions added
* `.pff_players_by_position()` low-level function added to pull player info from
grades endpoint by position
* `options(nflversepff.user_agent)` added to control user agent

