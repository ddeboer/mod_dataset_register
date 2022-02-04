%% @doc A module to register datasets with the Dataset Register provided by the Dutch Digital Heritage Network
%% (Netwerk Digitaal Erfgoed).
-module(mod_dataset_register).
-author("David de Boer <david@ddeboer.nl>").

-mod_title("Dataset Register").
-mod_description("Publish your datasets to the Dataset Register so they can be found and re-used by others").
-mod_prio(500).
-mod_depends([mod_ginger_rdf]).
-mod_schema(2).

-behaviour(gen_server).

-include_lib("zotonic.hrl").
-include_lib("mod_ginger_rdf/include/rdf.hrl").

-record(state, {context}).

-export([
    manage_schema/2,
    observe_rsc_to_rdf/3,
    pid_observe_rsc_update_done/3,
    init/1,
    handle_call/3,
    handle_cast/2,
    start_link/1
]).

manage_schema(_, Context) ->
    Datamodel = #datamodel{
        categories = [
            {dataset, query, [
                {title, <<"Dataset">>},
                {uri, rdf_property:schema(<<"Dataset">>)}
            ]}

        ]
    },
    z_datamodel:manage(?MODULE, Datamodel, Context).

observe_rsc_to_rdf(#rsc_to_rdf{id = Id}, Triples, Context) ->
    case m_rsc:is_a(Id, dataset, Context) of
        true ->
            Triples ++ m_dataset:dataset_to_rdf(Id, Context);
        false ->
            Triples
    end.

pid_observe_rsc_update_done(Pid, #rsc_update_done{} = Msg, _Context) ->
    gen_server:cast(Pid, Msg).

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    {ok, #state{context = z_context:new(Context)}}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.

handle_cast(#rsc_update_done{id = Id}, State = #state{context = Context}) ->
    case m_rsc:is_a(Id, dataset, Context) of
        true ->
            case dataset_register_client:validate(Id, Context) of
                valid ->
                    m_dataset:update_status(Id, valid, z_acl:sudo(Context));
                {invalid, ValidationResult} ->
                    ?DEBUG(ValidationResult),
                    m_dataset:update_status(Id, invalid, z_acl:sudo(Context))
            end;
        false ->
            noop
    end,
    {noreply, State};
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.
