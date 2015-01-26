import csv

def remove_last_row(iterable):
    iterator = iter(iterable)
    try:
        prev = next(iterator)
        while True:
            cur = next(iterator)
            yield prev
            prev = cur
    except StopIteration:
        return

def main():
	raw_data = csv.reader(open("performance_report_data.csv", "r"), delimiter=",")
	output_data = csv.writer(open("reporting_data.csv", "wb"), delimiter=",")

	for i, row in enumerate(remove_last_row(raw_data)):
		if i == 10:
			header = []
			for item in row:
				header.append(item.replace(" ", "").replace("-", "").replace(":", "_"))
			output_data.writerow(header)
		elif i > 10:
			output_data.writerow(row)
		else:
			continue
	print "Data formatted."

if __name__ == '__main__':
	main()

