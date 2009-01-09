-record(rule,
        {id,
         behavior,
         min=0,
         max=0,
         percent=0.0}).

-record(inspection,
        {id,
         stanza="all",
         rule,
         to,
         behavior,
         min=0,
         max=0,
         percent=0.0}).
