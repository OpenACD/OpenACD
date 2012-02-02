-record(simplekeyvalue, {
    key = erlang:error({required, key}),
    value = erlang:error({required, value})
}).

-record(skill, {
    atom = erlang:error({required, atom}),
    expanded
}).

-record(clientrecord, {
    is_default = erlang:error({required, is_default}),
    name,
    id,
    options = []
}).

-record(callerid, {
    name = "Unknown",
    data = "Unknown"
}).

-record(callrecord, {
    id = erlang:error({required, id}),
    type = erlang:error({required, type}),
    caller_id,
    dnis,
    client,
    ring_path = 'OUTBAND_RING',
    media_path = 'OUTBAND_PATH',
    direction = 'INBOUND',
    node,
    module_source = erlang:error({required, module_source}),
    arbitrary = []
}).

-record(release, {
    id = erlang:error({required, id}),
    name = erlang:error({required, name}),
    bias = erlang:error({required, bias})
}).

-record(agentrequest, {
    request_id = erlang:error({required, request_id}),
    request_hint = erlang:error({required, request_hint}),
    login_request,
    depricated_agent_transfer,
    dial_request,
    init_outbound_request,
    depricated_media_command_request,
    depricated_queue_transfer_request,
    depricated_warm_transfer_request,
    go_released_request,
    agent_client_version,
    set_endpoint_request,
    agent_channel_request,
    plugin_app,
    '$extensions' = dict:new()
}).

-record(agentclientversion, {
    major = erlang:error({required, major}),
    minor = erlang:error({required, minor})
}).

-record(loginrequest, {
    username = erlang:error({required, username}),
    password = erlang:error({required, password}),
    depricated_voipendpoint = 'SIP',
    depricated_voipendpointdata,
    depricated_use_outband_ring,
    depricated_use_persistent_ring,
    media_endpoints = []
}).

-record(setendpointrequest, {
    module_name = erlang:error({required, module_name}),
    endpoint_data
}).

-record(mediaendpointdata, {
    '$extensions' = dict:new()
}).

-record(mediaendpoint, {
    module_name = erlang:error({required, module_name}),
    '$extensions' = dict:new()
}).

-record(goreleasedrequest, {
    use_default = false,
    release_opt
}).

-record(statechange, {
    agent_state = erlang:error({required, agent_state}),
    call_record,
    release,
    client,
    warm_transfer_number,
    ssl_upgrade = false
}).

-record(agentchannelrequest, {
    request_hint = erlang:error({required, request_hint}),
    channel_id = erlang:error({required, channel_id}),
    agent_transfer_request,
    queue_transfer_request,
    media_command_request
}).

-record(mediacommandrequest, {
    need_reply = erlang:error({required, need_reply}),
    '$extensions' = dict:new()
}).

-record(agenttransferrequest, {
    agent_id = erlang:error({required, agent_id}),
    other_data
}).

-record(initoutboundrequest, {
    client_id = erlang:error({required, client_id}),
    media_type = erlang:error({required, media_type})
}).

-record(queuetransferrequest, {
    queue_name = erlang:error({required, queue_name}),
    transfer_options = erlang:error({required, transfer_options})
}).

-record(servermessage, {
    type_hint = erlang:error({required, type_hint}),
    reply,
    event
}).

-record(serverreply, {
    request_id = erlang:error({required, request_id}),
    request_hinted = erlang:error({required, request_hinted}),
    success = erlang:error({required, success}),
    error_message,
    error_code,
    release_opts = [],
    agents = [],
    queues = [],
    brands = [],
    profiles = [],
    queue_transfer_opts,
    media_command_reply,
    salt_and_key
}).

-record(saltreply, {
    salt = erlang:error({required, salt}),
    pubkey_e = erlang:error({required, pubkey_e}),
    pubkey_n = erlang:error({required, pubkey_n})
}).

-record(availagent, {
    name = erlang:error({required, name}),
    profile = erlang:error({required, profile}),
    state = erlang:error({required, state})
}).

-record(mediacommandreply, {
    '$extensions' = dict:new()
}).

-record(queuetransferoptions, {
    options = [],
    skills = []
}).

-record(serverevent, {
    command = erlang:error({required, command}),
    profile,
    state_change,
    depricated_call_data,
    text_message,
    depricated_number_dialed,
    depricated_client,
    depricated_url,
    depricated_url_window,
    depricated_media_event,
    url_pop_event,
    agent_channel_state_change,
    media_event,
    agent_channel_down,
    plugin_app
}).

-record(urlpopevent, {
    url = erlang:error({required, url}),
    window_id
}).

-record(agentchannelmediaevent, {
    channel_id = erlang:error({required, channel_id}),
    media_event
}).

-record(agentchannelstatechange, {
    statename = erlang:error({required, statename}),
    channel_id = erlang:error({required, channel_id}),
    call_record
}).

-record(mediaevent, {
    '$extensions' = dict:new()
}).

-record(depricated_warmtransferrequest, {
    number = erlang:error({required, number})
}).

-record(audiolevelrequest, {
    channel = erlang:error({required, channel}),
    value = erlang:error({required, value})
}).

-record(contact3rdpartyrequest, {
    target
}).

-record(merge3rdpartyrequest, {
    include_self = false
}).

-record(blindtransferrequest, {
    target = erlang:error({required, target})
}).

