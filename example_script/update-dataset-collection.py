import synapseclient
from synapseclient import Table, Row, RowSet, as_table_columns, File, Dataset
from synapseclient.models import Dataset
import pandas as pd
import time
import json

# -----------------------------
# Step 1: Login to Synapse
# -----------------------------
syn = synapseclient.Synapse()
syn.login()

# -----------------------------
# Step 2: Load syn50913342 and get dataset IDs
# -----------------------------
main_table_id = 'syn50913342'
main_table = syn.tableQuery(f"SELECT * FROM {main_table_id}")
main_df = main_table.asDataFrame()

# -----------------------------
# Step 3: Bind schema to each dataset
# -----------------------------
# -----------------------------
# Step 3: Bind full schema URI to each dataset
# -----------------------------
schema_id = "https://repo-prod.prod.sagebase.org//repo/v1/schema/type/registered/org.synapse.nf-portaldataset"

for objectId in main_df['id']:
    print(f"Binding schema to {objectId}...")
    
    # Construct the schema binding payload
    jsonSchemaObjectBinding = json.dumps({
        "entityId": objectId,
        "schema$id": schema_id
    })
    
    # Send the schema binding request to Synapse
    syn.restPUT(f"/entity/{objectId}/schema/binding", jsonSchemaObjectBinding)
    
    # Pause to avoid API rate limits
    time.sleep(0.5)

# -----------------------------
# Step 4: Load croissant_file_s3_object mapping from syn65903895
# -----------------------------
lookup_table_id = 'syn65903895'
lookup_table = syn.tableQuery(f"SELECT * FROM {lookup_table_id}")
lookup_df = lookup_table.asDataFrame()

# Assume `lookup_df` has columns: 'dataset', 'croissant_file_s3_object'
croissant_map = dict(zip(lookup_df['dataset'], lookup_df['croissant_file_s3_object']))

# -----------------------------
# Step 5: Annotate each dataset and snapshot it
# -----------------------------
updated_versions = {}

for objectId in main_df['id']:
    croissant_value = croissant_map.get(objectId)

    if croissant_value:
        print(f"🔄 Updating annotations for {objectId}...")

        try:
            # Step 1: Get current entity and annotations
            entity = syn.get(objectId, downloadFile=False)
            existing_annotations = syn.get_annotations(entity)
            print(f"📋 Current annotations for {objectId}: {existing_annotations}")

            # Step 2: Update annotations
            existing_annotations['croissant_file_s3_object'] = croissant_value
            updated_annotations = syn.set_annotations(annotations=existing_annotations)
            print(f"✅ Set annotations for {objectId}: {updated_annotations}")

            # Step 3: Snapshot the Dataset using model-based API
            ds = Dataset(id=objectId).get()
            snapshot_tx = ds.snapshot(
                comment="Annotated with croissant_file_s3_object",
                label="croissant update"
            )

            # Step 4: Track updated version from snapshot
            updated_versions[objectId] = snapshot_tx.snapshot_version_number

        except Exception as e:
            print(f"❌ Failed to annotate {objectId}: {e}")

    else:
        print(f"⚠️ No croissant metadata found for {objectId}, skipping.")

# -----------------------------
# Step 6: Update Main Table
