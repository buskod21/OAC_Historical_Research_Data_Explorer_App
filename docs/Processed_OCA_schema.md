---
layout: default  
title: Processed datasets in RED-X  
---

# Schema information
{: .no_toc }

## Table of Contents
{: .no_toc .text-delta }

1. TOC
{:toc}

**Name**: Processed datasets in RED-X  
**Description**: The processed_studies file contains a list of DOIs (Digital Object Identifiers) that have already been processed and cached within the RED-X app. It plays a crucial role in optimizing performance by allowing the application to skip reprocessing datasets that are already up-to-date. This helps reduce API load and improve app speed, especially when checking for new or updated studies from the Borealis Dataverse. In essence, it acts as a tracking list or checkpoint registry that the caching function (cache_raw_data()) uses to compare and filter out redundant operations.  
**Classification**: RDF212  
**Schema package SAID**: EIqXNdirNPd_vxjcyYXHKXblEOt21KKVb46LKohqg7jf  

## Schema quick view

| Attribute | Label | Description |
| --- | --- | --- |
| DOI | DOI | A unique persistent identifier for a research dataset. Used to verify whether the study has been processed. |

## International schema information

| Language | Name | Description |
| --- | --- | --- |
| English | Processed datasets in RED-X | The processed_studies file contains a list of DOIs (Digital Object Identifiers) that have already been processed and cached within the RED-X app. It plays a crucial role in optimizing performance by allowing the application to skip reprocessing datasets that are already up-to-date. This helps reduce API load and improve app speed, especially when checking for new or updated studies from the Borealis Dataverse. In essence, it acts as a tracking list or checkpoint registry that the caching function (cache_raw_data()) uses to compare and filter out redundant operations. |

## Language-independent schema details

| Attribute | Sensitive | Unit | Type | Character encoding | Required entry | Format rule |
| --- | --- | --- | --- | --- | --- | --- |
| DOI | false |  | Numeric | utf-8 | true | ^10\\\.\\d\{4,9\}/\[\-\.\_;\(\)/:A\-Z0\-9\]\+$ \(case\-insensitive\) |

## Language-specific schema details

### English

| Attribute | Label | Description | List |
| --- | --- | --- | --- |
| DOI | DOI | A unique persistent identifier for a research dataset. Used to verify whether the study has been processed. | Not a list |

## Schema SAIDs

**Capture base**: EBcWQH7PjQw6JiKG0X7FMFeUgMNv3SIstRf4MxhYzjeS

**Bundle**: ELcSYberEhPfMoYPgzUvX952e-G90g-8zJYCaY_US-5s

**Package**: EIqXNdirNPd_vxjcyYXHKXblEOt21KKVb46LKohqg7jf

| Layer | SAID | Type |
| --- | --- | --- |
| character_encoding | EGoNKnM8_drZa2f_5kGkkZJ9L2fjr6_2U-QQulUGApy2 | spec/overlays/character_encoding/1.1 |
| conformance | EKlhmc-JUaTyrcdWz1-Q1tAp1fbOMVfEhSdg6ZRsJ8j3 | spec/overlays/conformance/1.1 |
| format | EE9uIiziOm9eKomRq1XF-Zzr1Al8Zt23YgMoN6ERAmuv | spec/overlays/format/1.1 |
| information (eng) | ENeQ3DsBK2jl_3M3lFdEfBqHnBQjBbr077hZxrCzwTvs | spec/overlays/information/1.1 |
| label (eng) | EHJ39Oui6MI53ZnlHyfzvoRQliiTqzjps9fdSOFcdsim | spec/overlays/label/1.1 |
| meta (eng) | EEzuZlJb1mq2A2dC5vT8wM2ukJOebZtEM8ykbWKg1Dh5 | spec/overlays/meta/1.1 |
| ordering | EE6DknXqe1M6vGkbzslM__g4NkBVq0e1oT6vm2AXSHvP | community/overlays/adc/ordering/1.1 |

**Date created**: 2025-07-29 04:34:21

