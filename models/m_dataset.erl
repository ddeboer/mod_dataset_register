%% @doc Get RDF triples for datasets.
-module(m_dataset).

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    update_status/3,
    dataset_to_rdf/2,
    event/2
]).

-include_lib("zotonic.hrl").
-include_lib("mod_ginger_rdf/include/rdf.hrl").

m_find_value(Id, #m{value = undefined} = M, _Context) ->
    M#m{value = Id};
m_find_value(validation_url, #m{value = Id}, Context) ->
    dataset_register_client:validation_results_url(Id, Context).

m_to_list(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

m_value(_Arg0, _Arg1) ->
    erlang:error(not_implemented).

update_status(Id, Status, Context) when Status =:= invalid orelse Status =:= valid orelse Status =:= registered ->
    m_rsc:update(Id, [{dataset_register_validation, Status}], [no_touch], z_acl:sudo(Context)).

%% @doc Get dataset-specific RDF triples.
-spec dataset_to_rdf(m_rsc:resource(), z:context()) -> list(m_rdf:triple()).
dataset_to_rdf(Id, Context) ->
    organization(rdf_property:schema(<<"publisher">>), home, Context)
    ++ m_rdf_export:translations_to_rdf(rdf_property:schema(<<"name">>), m_rsc:p(Id, title, Context), Context)
    ++
    [
        #triple{
            predicate = rdf_property:schema(<<"version">>),
            object = #rdf_value{value = m_rsc:p(Id, version, Context)}
        },
        #triple{
            predicate = rdf_property:schema(<<"mainEntityOfPage">>),
            object = m_rsc:p(Id, uri, Context)
        },
        #triple{
            predicate = rdf_property:schema(<<"distribution">>),
            object = #rdf_resource{triples = [
                #triple{
                    predicate = rdf_property:rdf(<<"type">>),
                    object = rdf_property:schema(<<"DataDownload">>)
                },
                #triple{
                    predicate = rdf_property:schema(<<"encodingFormat">>),
                    object = #rdf_value{value = <<"application/ld+json">>}
                },
                #triple{
                    predicate = rdf_property:schema(<<"contentUrl">>),
                    object = z_dispatcher:url_for(rsc_json_ld, [{id, Id}, use_absolute_url], Context)
                }
            ]}
        },
        #triple{
            predicate = rdf_property:schema(<<"distribution">>),
            object = #rdf_resource{triples = [
                #triple{
                    predicate = rdf_property:rdf(<<"type">>),
                    object = rdf_property:schema(<<"DataDownload">>)
                },
                #triple{
                    predicate = rdf_property:schema(<<"encodingFormat">>),
                    object = #rdf_value{value = <<"text/turtle">>}
                },
                #triple{
                    predicate = rdf_property:schema(<<"contentUrl">>),
                    object = z_dispatcher:url_for(rsc_turtle, [{id, Id}, use_absolute_url], Context)
                }
            ]}
        }
    ].

-spec organization(m_rdf:predicate(), m_rsc:resource(), z:context()) -> list(m_rdf:triple()).
organization(Predicate, Id, Context) ->
    Iri = z_dispatcher:url_for(Id, [use_absolute_url], Context),
    [
        #triple{
            predicate = Predicate,
            object = Iri
        },
        #triple{
            subject = Iri,
            predicate = rdf_property:rdf(<<"type">>),
            object = rdf_property:schema(<<"Organization">>)
        },
        #triple{
            subject = Iri,
            predicate = rdf_property:schema(<<"name">>),
            object = #rdf_value{value = m_rsc:p(home, title, Context)}
        },
        #triple{
            subject = Iri,
            predicate = rdf_property:schema(<<"alternateName">>),
            object = #rdf_value{value = m_rsc:p(home, subtitle, Context)}
        }
    ].

event(#postback{message = {register_dataset, [{id, Id}]}}, Context) ->
    case dataset_register_client:submit(Id, Context) of
        valid ->
            m_rsc:update(
                Id, [
                    {registered, calendar:universal_time()},
                    {dataset_register_forbidden, undefined}
                ],
                Context
            );
        {invalid, 403} ->
            %% Toggle the property to re-enable the button without having to use wires.
            m_rsc:update(Id, [{dataset_register_forbidden, undefined}], Context),
            m_rsc:update(Id, [{dataset_register_forbidden, true}], Context);
        _ ->
            m_rsc:update(Id, [{dataset_register_forbidden, undefined}], Context)
    end,
    Context.
