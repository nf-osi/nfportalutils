from synapseclient import Synapse
from synapseclient.models import Dataset
import json

# -----------------------------
# Configuration
# -----------------------------
DATASET_COLLECTION_ID = "syn50913342"
APPLY_UPDATES = True   # <-- change to True to apply updates

# -----------------------------
# Login
# -----------------------------
syn = Synapse()
syn.login()

# -----------------------------
# Get dataset collection
# -----------------------------
collection = syn.restGET(f"/entity/{DATASET_COLLECTION_ID}")

print(f"\n📦 Auditing STABLE dataset versions in collection {DATASET_COLLECTION_ID}\n")
print("=" * 80)

collection_out_of_date_count = 0
no_stable_version_count = 0
updated_items = []

for item in collection["items"]:
    dataset_id = item["entityId"]
    collection_item_version = item["versionNumber"]

    # -----------------------------
    # Get dataset and compute STABLE latest version
    # -----------------------------
    ds = Dataset(id=dataset_id).get()
    draft_version = ds.version_number  # ✅ correct attribute

    if draft_version < 2:
        stable_latest_version = None
        no_stable_version_count += 1
    else:
        stable_latest_version = draft_version - 1

    # -----------------------------
    # Compare stable latest vs collection version
    # -----------------------------
    collection_outdated = (
        stable_latest_version is not None
        and collection_item_version != stable_latest_version
    )

    if collection_outdated:
        collection_out_of_date_count += 1

    # -----------------------------
    # Print report
    # -----------------------------
    print(f"Dataset: {dataset_id}")
    print(f"  • Draft latest version        : {draft_version}")
    print(f"  • Stable latest version       : {stable_latest_version}")
    print(f"  • Collection item version     : {collection_item_version}")

    if stable_latest_version is None:
        print("  ⚠️  No stable version exists yet (draft only)")
        updated_items.append(item)  # unchanged
    elif collection_outdated:
        print("  ❌ Collection version does NOT match stable latest")

        # Prepare updated collection item
        updated_items.append({
            "entityId": dataset_id,
            "versionNumber": stable_latest_version
        })
    else:
        print("  ✅ Collection version matches stable latest")
        updated_items.append(item)

    print("-" * 80)

print(
    f"\nSummary:\n"
    f"  • {collection_out_of_date_count} dataset(s) with collection version out of sync\n"
    f"  • {no_stable_version_count} dataset(s) without a stable version\n"
    f"  • {len(collection['items'])} total dataset(s) audited\n"
)

# -----------------------------
# Apply updates (optional)
# -----------------------------
if APPLY_UPDATES and collection_out_of_date_count > 0:
    print("🚀 Applying updates to dataset collection...")

    collection["items"] = updated_items

    syn.restPUT(
        f"/entity/{DATASET_COLLECTION_ID}",
        json.dumps(collection)
    )

    print("✅ Dataset collection updated to stable latest versions.")

elif APPLY_UPDATES:
    print("ℹ️ No updates required. Collection already in sync.")

else:
    print("🧪 Dry run only — no changes applied.")
