music_queries = new Meteor.Collection("music_queries");
device_status = new Meteor.Collection("device_status");

if (Meteor.isClient) {
	Meteor.startup(function () {
		var initialStatus = { volume : -1, state : "stop", song : null, elapsed : null };
		Session.set("query_id", null);
		var mainDiv = document.getElementById("main");
		var missioncontrol = Elm.embed(Elm.Main, mainDiv, {
			searchQueryResult : null,
			musicStatus : initialStatus
		});

		Deps.autorun(function () {
			var query_id = Session.get("query_id");
			console.log(query_id);
			var music_query = music_queries.findOne({ _id : query_id });
			console.log(music_query);
			if (music_query) {
				switch (music_query.command) {
					case "search":
						missioncontrol.ports.searchQueryResult.send(music_query);
						break;
					case "status":
						missioncontrol.ports.musicStatus.send(music_query.result);
				}
			}
		});

		Deps.autorun(function () {
			var status = device_status.findOne({ device_name : "music" });
			console.log(status);
			if (status) {
				missioncontrol.ports.musicStatus.send(status);
			}
		});

		missioncontrol.ports.searchQueryOut.subscribe(function (query) {
			if (query) {
				var music_query = { command : "search", arguments : ["title", query], result : [], waiting : true }
				var query_id = music_queries.insert(music_query);
				Session.set("query_id", query_id);
			}
		});

		missioncontrol.ports.songCommandOut.subscribe(function (command) {
			if (command) {
				var music_query = { command : "raw_command", arguments : [command], result : [], waiting : true }
				music_queries.insert(music_query);
			}
		});

		missioncontrol.ports.nextSongOut.subscribe(function (song) {
			if (song) {
				var music_query = { command : "enqueue", arguments : [song], result : [], waiting : true }
				music_queries.insert(music_query);
			}
		})
	});
}

if (Meteor.isServer) {}
