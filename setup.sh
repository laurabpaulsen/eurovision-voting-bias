# create venv
python3 -m venv esc-env

# activate venv
source esc-env/bin/activate

# install requirements from eurovision-data repo
pip install -r eurovision-dataset/requirements.txt

# install requirements from this repo
pip install -r requirements.txt
