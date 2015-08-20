# Default time a notification is shown (ms)
DEFAULT_NOTIFY_TIMEOUT = 6000

# Maximum time a notification is allowed to show (ms)
MAX_NOTIFY_TIMEOUT = 10000

# Maximum number of characters in a notification
NOTIFICATION_MAX_LENGTH = 80

# Time between regular status updates (s)
STATUS_UPDATE_INTERVAL = 6.0

# Command from which to fetch status text (reads back from stdout)
import os
STATUS_COMMAND = ['/bin/sh', '%s/.bspwm/statnot.sh' % os.getenv('HOME')] 

# Always show text from STATUS_COMMAND? If false, only show notifications
USE_STATUSTEXT=True

# Put incoming notifications in a queue, so each one is shown?
# If false, the most recent notification is shown directly.
QUEUE_NOTIFICATIONS=True

# update_text(text) is called when the status text should be updated
# If there is a pending notification to be formatted, it is appended as
# the final argument to the STATUS_COMMAND, e.g. as $1 in default shellscript
def update_text(text):
        print text

##
