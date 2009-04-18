-module(trace_test).

-export([test/0,start_trace/0]).

-include_lib("et/include/et.hrl").

test() ->
  {ok,Pid} = et_viewer:start([{trace_global,true},{hide_unknown,true},{trace_pattern,{et,max}},{max_actors,10}]),
  true = register(my_et_viewer,Pid),
  true = register(my_et_collector,et_viewer:get_collector_pid(Pid)),
  start_trace(),
  et:report_event(90,agent,agent,init_start,undefined),
  et:report_event(50,agent,conn_mux,init,undefined),
  et:report_event(50,conn_mux,agent,ready,undefined),
  et:report_event(80,agent,xmpp,connect,undefined),
  et:report_event(90,agent,agent,init_done,undefined),
  et:report_event(90,agent,agent,core_done,undefined),
  et:report_event(80,agent,actor_core,load,undefined),
  et:report_event(80,agent,actor_test,load,undefined),
  et:report_event(80,actor_core,job_advert,start,undefined),
  et:report_event(80,job_advert,actor_core,ready,undefined),
  et:report_event(80,job_discover,actor_core,ready,undefined),
  et:report_event(80,actor_core,job_discover,start,undefined),
  et:report_event(80,actor_core,agent,ready,undefined),
  et:report_event(90,agent,agent,core_done,undefined),
  et:report_event(90,agent,agent,actors_init,undefined),
  et:report_event(70,job_advert,job_advert,update_adverts,[{adverts,[]}]),
  et:report_event(70,actor_test,job_advert,update_ads,undefined),
  et:report_event(70,actor_test,agent,ready,undefined),
  et:report_event(90,agent,agent,actors_done,undefined),
  et:report_event(90,agent,agent,'running',undefined),
  et:report_event(70,job_advert,job_advert,update_adverts,[{adverts,[{adv,3600,op,"/test/foo"},{adv,3600,res,"/blue/bonnet"}]}]),
  et:report_event(60,job_advert,conn_mux,outcall,[{token,12345},{op,"/security/advertise"}]),
  et:report_event(50,conn_mux,xmpp,init,[{token,12345}]),
  et:report_event(80,xmpp,agent,connected,[testuser,'server.com',res]),
  et:report_event(50,xmpp,conn_mux,ack,[{token,12345}]),
  et:report_event(60,conn_mux,job_advert,outcall_start,undefined),
  et:report_event(50,xmpp,conn_mux,data,[{token,12345}]),
  et:report_event(60,conn_mux,job_advert,outcall_data,undefined),
  et:report_event(50,xmpp,conn_mux,final,[{token,12345}]),
  et:report_event(60,conn_mux,job_advert,outcall_done,undefined).

start_trace() ->
  dbg:p(all,call),
  dbg:tpl(et, report_event, 5, []).
