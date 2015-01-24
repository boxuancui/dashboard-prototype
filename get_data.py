import config
import sys
import csv
import requests

from apiclient import sample_tools
from oauth2client import client
from pprint import pprint

def get_userprofiles(argv):
	# Authenticate and construct service
	service, _ = sample_tools.init(argv, "dfareporting", "v2.0", __doc__, __file__, parents=[], scope=config.scope)
	
	try:
		# Construct the request
		request = service.userProfiles().list()
		# Execute request and print response
		response = request.execute()
		for profile in response["items"]:
			print "Found user profile with ID %s and user name %s." % (profile["profileId"], profile["userName"])
	except client.AccessTokenRefreshError:
		print "The credentials have been revoked or expired, please re-run the application to re-authorize."

	# config.profile_id = raw_input("Enter profile ID: ")
	print "Profile ID", config.profile_id, "selected."


def get_reports(argv, profile_id):
	# Authenticate and construct service
	service, _ = sample_tools.init(argv, "dfareporting", "v2.0", __doc__, __file__, parents=[], scope=config.scope)
	
	try:
		# Construct the request
		request = service.reports().list(profileId=profile_id)
		while True:
			# Execute request and print response
			response = request.execute()
			for report in response["items"]:
				print ("Found %s report with ID %s and name %s." % (report["type"], report["id"], report["name"]))
			if response['items'] and response['nextPageToken']:
				request = service.reports().list_next(request, response)
			else:
				break
	except client.AccessTokenRefreshError:
		print "The credentials have been revoked or expired, please re-run the application to re-authorize."

	# config.report_id = raw_input("Enter report ID: ")
	print "Report ID", config.report_id, "selected."


def run_report(argv, profile_id, report_id):
	# Authenticate and construct service
	service, _ = sample_tools.init(argv, "dfareporting", "v2.0", __doc__, __file__, parents=[], scope=config.scope)
	
	try:
		# Construct a get request for the specified report
		request = service.reports().run(profileId=profile_id, reportId=report_id)
		# Execute request and print response
		result = request.execute()
		print "Running report file with ID %s." % result["id"]
		config.file_id = result["id"]

		status_output = 0
		while status_output == 0:
			# Construct a get request for report file status
			request_status = service.reports().files().list(profileId=profile_id, reportId=report_id).execute()
			report_file = request_status["items"][0]
			if report_file["status"] != "REPORT_AVAILABLE":
				sys.stdout.write("Current status: %s. \r" % report_file["status"])
				sys.stdout.flush()
			else:
				status_output = 1
				sys.stdout.write("Current status: %s. \r" % report_file["status"])
				sys.stdout.flush()
	except client.AccessTokenRefreshError:
		print "The credentials have been revoked or expired, please re-run the application to re-authorize."

def download_file(argv, report_id, file_id):
	# Authenticate and construct service
	service, _ = sample_tools.init(argv, "dfareporting", "v2.0", __doc__, __file__, parents=[], scope=config.scope)
	
	try:
		# Initiate .csv file
		data_file = open("performance_report_data.csv", "wb")
		# Construct the request
		request = service.files().get_media(reportId=report_id, fileId=file_id)
		# Execute request and print the file contents
		response = request.execute()
		data_file.write(response)
		print "Report download complete!"
	except client.AccessTokenRefreshError:
		print "The credentials have been revoked or expired, please re-run the application to re-authorize."



if __name__ == '__main__':
	print "Connecting to Google DCM/DFA Reporting and Trafficking API ..."
	print
	get_userprofiles(sys.argv)
	print
	get_reports(sys.argv, config.profile_id)
	print
	run_report(sys.argv, config.profile_id, config.report_id)
	print
	print 
	download_file(sys.argv, config.report_id, config.file_id)



