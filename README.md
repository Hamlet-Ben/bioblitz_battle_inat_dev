# bioblitz_battle_inat_dev
A development version of our iNat based bioblitz battle game, which uses the dummy database (without personal info)

This app is used to provide users with live score updates for bioblitz battles they are participating in. Data is collected via the iNaturalist app and is assigned to an iNaturalist 'project' for each team. The bioblitz battle app then pulls the data from iNaturalist from these project and assigns scores to each species by matching species names to a species score list. Scores in this list are based on rarity, with rarer species receiving higher scores. Once scores are assigned to records, team scores are calculated and presented in a league table.

The Bioblitz Battle app is RShiny web app built specific for mobile devices using the shinyMobile package. It uses the RMariaDB package to access the RPP database and the rinat package to download data from iNaturalist (although I have actually adapted the rinat functions myself since they had some problems initially).

This is one of a few iterations of app I and other team members have built for this purpose and the first to be successful enough for regular use. I think the reason why this one works and others have failed is that the other weren't built specifically for mobile devices.
