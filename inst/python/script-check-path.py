import os
import os.path


def check_path_py(file_path):
    # Verify the path is valid
    check_file = os.path.isfile(file_path) and os.path.exists(file_path)
    return(check_file)
