%% @doc A client for the Dataset Register REST API (https://datasetregister.netwerkdigitaalerfgoed.nl/api).
-module(dataset_register_client).
-author("David de Boer <david@ddeboer.nl>").

-export([
    validate/2,
    validation_results_url/2
]).

-include("zotonic.hrl").

-define(URL, <<"https://datasetregister.netwerkdigitaalerfgoed.nl/api/">>).
%%-define(URL, <<"http://localhost:3000">>).
-define(VALIDATE_URL, <<"https://datasetregister.netwerkdigitaalerfgoed.nl/validate.php?url=">>).

%% @doc Validate a dataset description with the Dataset Register.
%% @see https://netwerk-digitaal-erfgoed.github.io/requirements-datasets/
-spec validate(m_rsc:resource(), z:context()) -> valid | {invalid, 404 | 406 | map()}.
validate(Id, Context) ->
    Payload = #{<<"@id">> => dataset_uri(Id, Context)},
    case httpc:request(
        put, {
            binary_to_list(<<?URL/binary, "/datasets/validate">>),
            [],
            "application/ld+json",
            jsx:encode(Payload)
        },
        httpc_options(), []
    ) of
        {ok, {{_, 200, _}, _Headers, _}} ->
            %% Dataset description is valid.
            valid;
        {ok, {{_, 404, _}, _Headers, _}} ->
            %% URL does not exist.
            {invalid, 404};
        {ok, {{_, 400, _}, _Headers, Body}} ->
            %% Dataset description is invalid.
            {invalid, jsx:decode(list_to_binary(Body))};
        {ok, {{_, 406, _}, _Headers, _}} ->
            %% No dataset can be found at the URL.
            {invalid, 406}
    end.

-spec validation_results_url(m_rsc:resource(), z:context()) -> binary().
validation_results_url(Id, Context) ->
    <<?VALIDATE_URL/binary,
        (z_convert:to_binary(z_url:url_encode(dataset_uri(Id, Context))))/binary>>.

-spec dataset_uri(m_rsc:resource(), z:context()) -> binary().
dataset_uri(Id, Context) ->
    m_rsc:p(Id, uri, Context).

httpc_options() ->
    [
        {timeout, 10000},
        {connect_timeout, 5000}
    ].
