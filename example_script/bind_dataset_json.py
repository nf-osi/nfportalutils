import synapseclient
import time
import json

# Initialize Synapse client and login
def initialize_synapse():
    syn = synapseclient.Synapse()
    syn.login()  # Add your username and password if needed
    return syn

# Define the schema binding function
def bind_schema(syn, object_id, schema_id):
    print(f"Binding schema '{schema_id}' to entity '{object_id}'...")
    bind_request = {
        "entityId": object_id,
        "schema$id": schema_id
    }
    syn.restPUT(f"/entity/{object_id}/schema/binding", json.dumps(bind_request))
    print("Schema successfully bound to the entity.")

# Main function to run the script
def main():
    syn = initialize_synapse()


    # # Create schema
    # job_status = create_schema(syn, schema_request_body)

    # Object IDs to bind schema to
    object_ids = [
        "syn52369033"
    ]  # Replace with actual entity IDs

    schema_id = "https://repo-prod.prod.sagebase.org/repo/v1/schema/type/registered/org.synapse.nf-superdataset"

    # Bind each schema
    for object_id in object_ids:
        bind_schema(syn, object_id, schema_id)

if __name__ == "__main__":
    main()
