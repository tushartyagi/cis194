parse m = case words m of
  ("E":severity:time:message) -> LogMessage Error severity time message 
  ("I":time:message) -> LogMessage Info time message
  ("W":time:message) -> LogMessage Warning time message
  _ -> Unknown _
