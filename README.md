mod_dataset_register
====================

A Zotonic module to publish dataset descriptions to the
[Dataset Register](https://datasetregister.netwerkdigitaalerfgoed.nl/), an index of datasets that are available in The
Netherlands, intended to make it easier to find and use those datasets.

You can find technical usage documentation at
the [Dataset Register code repository](https://github.com/netwerk-digitaal-erfgoed/dataset-register).

Features:

* Validate dataset descriptions with the Dataset Register.
* Publish dataset descriptions to the Dataset Register.

## Usage

Enabling this module adds a `dataset` category. Published resources in this category will be automatically
[validated with the Dataset Register](https://datasetregister.netwerkdigitaalerfgoed.nl/api/static/index.html#/default/validate)
when they are updated.

In the admin, a [status bar](templates/dataset-status.tpl) on the dataset edit page shows whether it conforms to the
[Requirements for Datasets](https://netwerk-digitaal-erfgoed.github.io/requirements-datasets/). If it does, the status
bar shows a Register button that will
[publish the dataset description to the Dataset Register](https://datasetregister.netwerkdigitaalerfgoed.nl/api/static/index.html#/default/post_datasets).

The status bar may also show a warning if the site’s domain name needs to be added to the [Register’s allow list](https://triplestore.netwerkdigitaalerfgoed.nl/resource?uri=https:%2F%2Fdata.netwerkdigitaalerfgoed.nl%2Fregistry%2Fallowed_domain_names&role=context).

When the dataset description is published, it can be [found by others](https://datasetregister.netwerkdigitaalerfgoed.nl/)
that are interested in your data.

## Models

### m_dataset

The [m_dataset](models/m_dataset.erl) model takes care of serializing the dataset resource to RDF according to the
[Requirements for Datasets](https://netwerk-digitaal-erfgoed.github.io/requirements-datasets/#dataset-example).
To do so, this module hooks into mod_ginger_rdf’s
[`#rsc_to_rdf{}`](https://github.com/driebit/ginger/tree/master/modules/mod_ginger_rdf#changing-the-rdf-representation)
notification.
