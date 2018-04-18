# add progress to ensure Travis doesn't complain about no output
while true; do
  sleep 30
  if jobs -rp | grep ${PID} >/dev/null; then
     echo "."
  else
     echo
     break
fi
done
