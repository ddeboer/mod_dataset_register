{# Only published resources can be submitted to the Dataset Register. #}
{% if id.is_published %}
    <div class="dataset-status">
        {% if id.registered and id.dataset_register_validation == "valid" %}
            <div class="alert alert-info" role="alert">
                <svg class="icon" width="18" height="18" role="img" aria-label="Info:"><use xlink:href="#check-circle-fill"/></svg>
                <div>
                    {_ This dataset has been successfully submitted to the _}
                    <a target="_blank" class="alert-link" href="https://datasetregister.netwerkdigitaalerfgoed.nl/">{_ Dataset Register _}<span class="glyphicon glyphicon-new-window"></span></a>.
                </div>
            </div>
        {% elseif id.dataset_register_forbidden %}
            <div class="alert alert-danger" role="alert">
                <svg class="icon" width="18" height="18" role="img" aria-label="Info:"><use xlink:href="#exclamation-triangle-fill"/></svg>
                <div>
                    {_ This site’s domain name is not yet allowed to register datasets. _}
                    {_ Contact <a href="mailto:info@nationaalarchief.nl" class="alert-link">info@nationaalarchief.nl</a> to add _}
                    <samp>{{ m.site.hostname }}</samp>
                    {_ to the allow list and try again. _}
                </div>
                {% button
                    id="submit-button"
                    class="btn btn btn-danger"
                    text=_"Submit to Dataset Register"
                    action={script script="$(event.currentTarget).attr('disabled', 'disabled')"}
                    postback={register_dataset id=id} delegate=`m_dataset`
                %}
            </div>
        {% elseif id.dataset_register_validation == "valid" %}
            <div class="alert alert-success" role="alert">
                <svg class="icon" width="18" height="18" role="img" aria-label="Info:"><use xlink:href="#info-fill"/></svg>
                <div>
                    {_ This dataset is valid, so you can submit it to the _}
                    <a target="_blank" class="alert-link" href="https://datasetregister.netwerkdigitaalerfgoed.nl/">{_ Dataset Register _}<span class="glyphicon glyphicon-new-window"></span></a>.
                </div>
                {% button class="btn btn btn-success" text=_"Submit to Dataset Register" postback={register_dataset id=id} delegate=`m_dataset` %}
            </div>
        {% elseif id.dataset_register_validation == "invalid" %}
            <div class="alert alert-warning" role="alert">
                <svg class="icon" width="18" height="18" role="img" aria-label="Info:"><use xlink:href="#exclamation-triangle-fill"/></svg>
                <div>
                    {_ This dataset is not yet valid. Make sure to enter values for all required fields. _}
                    <a target="_blank" class="alert-link" href="{{ m.dataset[id].validation_url }}">{_ View validation results _}<span class="glyphicon glyphicon-new-window"></span></a>
                </div>
            </div>
        </div>
        {% endif %}
    </div>

    <svg xmlns="http://www.w3.org/2000/svg" style="display: none;">
        <symbol id="check-circle-fill" fill="currentColor" viewBox="0 0 16 16">
            <path d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-3.97-3.03a.75.75 0 0 0-1.08.022L7.477 9.417 5.384 7.323a.75.75 0 0 0-1.06 1.06L6.97 11.03a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 0 0-.01-1.05z"/>
        </symbol>
        <symbol id="info-fill" fill="currentColor" viewBox="0 0 16 16">
            <path d="M8 16A8 8 0 1 0 8 0a8 8 0 0 0 0 16zm.93-9.412-1 4.705c-.07.34.029.533.304.533.194 0 .487-.07.686-.186l-.088.416c-.287.346-.92.598-1.465.598-.703 0-1.002-.422-.808-1.319l.738-3.468c.064-.293.006-.399-.287-.47l-.451-.081.082-.381 2.29-.287zM8 5.5a1 1 0 1 1 0-2 1 1 0 0 1 0 2z"/>
        </symbol>
        <symbol id="exclamation-triangle-fill" fill="currentColor" viewBox="0 0 16 16">
            <path d="M8.982 1.566a1.13 1.13 0 0 0-1.96 0L.165 13.233c-.457.778.091 1.767.98 1.767h13.713c.889 0 1.438-.99.98-1.767L8.982 1.566zM8 5c.535 0 .954.462.9.995l-.35 3.507a.552.552 0 0 1-1.1 0L7.1 5.995A.905.905 0 0 1 8 5zm.002 6a1 1 0 1 1 0 2 1 1 0 0 1 0-2z"/>
        </symbol>
    </svg>

    {% lib
        "css/dataset-register-admin.css"
    %}
{% endif %}
