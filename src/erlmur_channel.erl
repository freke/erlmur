-module(erlmur_channel).

-export(
	[
		new/1,
    name/1,
    channel_id/1,
		parent/1,
    parent/2,
    link/2,
    unlink/2,
		linked/1
  ]).

-include("erlmur_channel.hrl").

-opaque channel() :: #channel{}.
-export_type([channel/0]).

-spec new(root|string()) -> channel().
new(root) ->
  #channel{
    name="Root",
    channel_id=0
  };
new(Name) ->
  ChannelId = erlang:phash2({?MODULE, erlang:timestamp()}),
  #channel{
    name=Name,
    channel_id=ChannelId
  }.

-spec name(channel()) -> string().
name(Channel) ->
  Channel#channel.name.

-spec channel_id(channel()) -> integer().
channel_id(Channel) ->
  Channel#channel.channel_id.

-spec parent(channel()) -> integer().
parent(Channel) ->
	Channel#channel.parent.

-spec parent(integer(), channel()) -> channel().
parent(ParentId, Channel) ->
	Channel#channel{parent=ParentId}.

-spec link(integer(), channel()) -> channel().
link(ChannelId, Channel) ->
  NewLinks=lists:umerge([ChannelId], Channel#channel.links),
  Channel#channel{links=NewLinks}.

-spec unlink(integer(), channel()) -> channel().
unlink(ChannelId, Channel) ->
  NewLinks = lists:subtract(Channel#channel.links, [ChannelId]),
  Channel#channel{links=NewLinks}.

-spec linked(channel()) -> [integer()].
linked(Channel) ->
	Channel#channel.links.
