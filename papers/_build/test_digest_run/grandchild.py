"""Stands in for an MCP server: inherits the run's stdout/stderr write handles and outlives its parent."""
import sys, time

time.sleep(float(sys.argv[1]))
