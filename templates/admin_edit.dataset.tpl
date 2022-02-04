{% extends "admin_edit.tpl" %}

{% block admin_edit_form_top %}
    {% live template="dataset-status.tpl" id=id topic="~site/rsc/"++id %}
{% endblock %}
