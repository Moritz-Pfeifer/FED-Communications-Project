import json


def load_settings(_relative_path):
    settings = {}
    with open(_relative_path) as f:
        settings = json.load(f)

    f.close()
    return settings
