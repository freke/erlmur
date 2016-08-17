-ifndef(erlmur).
-define(erlmur, true).
  -record(channel,
    {
      channel_id,
      parent,
      name,
      links=[]
    }).
-endif.
