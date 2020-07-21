import progressbar
from lxml import etree
import datetime
import dateutil
import pdb
import json

# Two dimension exchange rate dictionary. Access exchange rates by currency and year like ratedf[currencyCode][year]
with open('ex_rates.json') as f:
    ratedf = json.load(f)

ratedf["GPB"] = ratedf["GBP"]
ratedf["gbp"] = ratedf["GBP"]
ratedf["EURO"] = ratedf["EUR"]
ratedf["Euro"] = ratedf["EUR"]
ratedf["Eur"] = ratedf["EUR"]
ratedf["CDN"] = ratedf["CAD"]
ratedf["usd"] = ratedf["USD"]
ratedf["GHC"] = ratedf["GHS"]
ratedf["ZMK"] = ratedf["ZMW"]
ratedf["USS"] = ratedf["USD"]
ratedf["USN"] = ratedf["USD"]
ratedf["BEF"] = ratedf["EUR"]
ratedf["FIM"] = ratedf["EUR"]
ratedf["KSH"] = ratedf["KES"]
ratedf["GIP"] = ratedf["GBP"]
ratedf["FKP"] = ratedf["GBP"]
ratedf["AON"] = ratedf["AOA"]
ratedf["UYI"] = ratedf["UYU"]
ratedf["NUL"] = {"2000": 0}


# Used for ambiguously structed arrays resulting from XML queries. If an array has any entries, take the first one.
def default_first(array):
    # If an array isn't empty, give us the first element
    return array[0] if array is not None and len(array) > 0 else None


# Used for ambiguous result default replacement. If value doesn't exist, replace it with the default.
def replace_default_if_none(value, default):
    if value is None:
        return default
    elif str.strip(value) == "":
        return default
    else:
        return value


# Used for ambiguous recoding. If code exists, try and use the dictionary to look up the result.
def recode_if_not_none(code, dictionary):
    if code is None:
        return None
    elif str.strip(code) == "":
        return None
    else:
        try:
            return dictionary[code]
        except KeyError:
            return None


# Used for currency conversion. Works like recode_if_not_none but for our 2-dimension exchange rate dictionary
def convert_usd(value, year, currency, ratedf):
    if value == 0:
        return 0
    elif value is None or year is None or currency is None:
        return None
    try:
        conversion_factor = ratedf[currency][str(year)]
        if conversion_factor > 0:
            return value*conversion_factor
        else:
            return None
    except KeyError:
        return None


# A class that will hold the flattening function and dictionary definitions
class IatiFlat(object):
    def __init__(self):
        self.header = ["year", "transaction_date", "budget_period_start", "budget_period_end", "transaction_type", "usd_disbursement", "budget_or_transaction", "budget_type", "iati_identifier", "reporting_org_name", "reporting_org_ref", "reporting_org_type", "secondary_reporter", "humanitarian", "transaction_sector_code", "transaction_sector_percentage", "transaction_sector_vocabulary", "recipient_country_codes", "recipient_country_percentages", "finance_type_code", "humanitarian_scope_narrative", "humanitarian_scope_code", "activity_title", "activity_description", "transaction_description_narrative", "tag_code", "tag_narrative", "participating_org_name", "participating_org_ref", "participating_org_type", "participating_org_role"]
        self.dictionaries = {}
        # Defaults, can be overwritten with next function
        self.dictionaries["ratedf"] = ratedf

    def define_dict(self, name, dictionary):
        self.dictionaries[name] = dictionary

    # Main flattening function here. Input is the XML root of the XML document, and output is an array of arrays with flattened data.
    def flatten_activities(self, root):
        for dictionary_name in ["ratedf"]:
            assert dictionary_name in self.dictionaries, "Missing dictionary: {}".format(dictionary_name)
        output = []
        try:
            version = root.attrib["version"]
        except KeyError:
            # Defaults to 2.02 if  the document happens to be missing an IATI version
            version = '2.02'

        # Find all activities
        activity_len = len(root.findall("iati-activity"))

        # Set up a quick progress bar for tracking processing; iterate through every activity
        bar = progressbar.ProgressBar()
        for i in bar(range(0, activity_len)):
            activity = root.xpath('iati-activity[%s]' % (i + 1))[0]
            # Capture iati identifier
            iati_identifier = default_first(activity.xpath("iati-identifier/text()"))

            child_tags = [child.tag for child in activity.getchildren()]

            secondary_reporter = default_first(activity.xpath("reporting-org/@secondary-reporter"))
            secondary_reporter = replace_default_if_none(secondary_reporter, "0")

            reporting_org_type = default_first(activity.xpath("reporting-org/@type"))

            humanitarian = default_first(activity.xpath("@humanitarian"))
            humanitarian_scope_narrative = default_first(activity.xpath("humanitarian-scope/narrative/text()"))
            humanitarian_scope_code = default_first(activity.xpath("humanitarian-scope/@code"))
            activity_title = default_first(activity.xpath("title/narrative/text()"))
            activity_description = default_first(activity.xpath("description/narrative/text()"))
            tag_code = default_first(activity.xpath("tag/@code"))
            tag_narrative = default_first(activity.xpath("tag/narrative/text()"))

            recipient_country_codes = []
            recipient_country_percentages = []
            recipient_countries = activity.findall("recipient-country")
            for recipient_country in recipient_countries:
                attribs = recipient_country.attrib
                attrib_keys = list(attribs.keys())
                percentage = attribs['percentage'] if 'percentage' in attrib_keys else "100"
                if percentage is not None:
                    percentage = percentage.replace("%", "")
                code = attribs['code'] if 'code' in attrib_keys else None
                if code is not None:
                    recipient_country_codes.append(code)
                    recipient_country_percentages.append(percentage)
            recipient_country_codes = ",".join(recipient_country_codes)
            recipient_country_percentages = ",".join(recipient_country_percentages)

            activity_sector_codes = []
            activity_sector_percentages = []
            activity_sector_vocabularies = []
            activity_sectors = activity.findall("sector")
            for activity_sector in activity_sectors:
                attribs = activity_sector.attrib
                attrib_keys = list(attribs.keys())
                percentage = attribs['percentage'] if 'percentage' in attrib_keys else "100"
                if percentage is not None:
                    percentage = percentage.replace("%", "")
                if percentage is None:
                    percentage = ""
                vocab = attribs['vocabulary'] if 'vocabulary' in attrib_keys else None
                if vocab is None:
                    vocab = ""
                code = attribs['code'] if 'code' in attrib_keys else None
                if code is not None:
                    activity_sector_codes.append(code)
                    activity_sector_percentages.append(percentage)
                    activity_sector_vocabularies.append(vocab)
            activity_sector_codes = ",".join(activity_sector_codes)
            activity_sector_percentages = ",".join(activity_sector_percentages)
            activity_sector_vocabularies = ",".join(activity_sector_vocabularies)

            reporting_org_name = default_first(activity.xpath("reporting-org/narrative/text()"))
            reporting_org_ref = default_first(activity.xpath("reporting-org/@ref"))

            participating_org_ref = []
            participating_org_type = []
            participating_org_role = []
            participating_org_name = []
            participating_orgs = activity.findall("participating-org")
            for participating_org in participating_orgs:
                attribs = participating_org.attrib
                attrib_keys = list(attribs.keys())
                ref = attribs['ref'] if 'ref' in attrib_keys else ""
                participating_org_ref.append(ref)
                p_type = attribs['type'] if 'type' in attrib_keys else ""
                participating_org_type.append(p_type)
                role = attribs['role'] if 'role' in attrib_keys else ""
                participating_org_role.append(role)
                p_name = default_first(participating_org.xpath("narrative/text()"))
                p_name = p_name if p_name else ""
                participating_org_name.append(p_name)
            participating_org_ref = ",".join(participating_org_ref)
            participating_org_type = ",".join(participating_org_type)
            participating_org_role = ",".join(participating_org_role)
            participating_org_name = ",".join(participating_org_name)

            defaults = {}
            default_tags = ["default-currency", "default-finance-type"]
            for tag in default_tags:
                if tag in activity.attrib.keys():
                    defaults[tag] = activity.attrib[tag]
                elif tag in child_tags:
                    defaults[tag] = default_first(activity.xpath("{}/@code".format(tag)))
                else:
                    defaults[tag] = None

            has_transactions = "transaction" in child_tags
            has_budget = "budget" in child_tags
            if not has_transactions and not has_budget:
                activity_date = default_first(activity.xpath("activity-date/@iso-date"))
                year = activity_date[:4] if activity_date is not None else None
                row = [year, activity_date, None, None, None, None, "Activity", None, iati_identifier, reporting_org_name, reporting_org_ref, reporting_org_type, secondary_reporter, humanitarian, activity_sector_codes, activity_sector_percentages, activity_sector_vocabularies, recipient_country_codes, recipient_country_percentages, None, humanitarian_scope_narrative, humanitarian_scope_code, activity_title, activity_description, None, tag_code, tag_narrative, participating_org_name, participating_org_ref, participating_org_type, participating_org_role]
                output.append(row)
            if has_transactions:
                transactions = activity.findall("transaction")

                for transaction in transactions:
                    finance_type_code = default_first(transaction.xpath("finance-type/@code"))
                    finance_type_code = replace_default_if_none(finance_type_code, defaults["default-finance-type"])

                    transaction_type_code = default_first(transaction.xpath("transaction-type/@code"))
                    transaction_date = default_first(transaction.xpath("transaction-date/@iso-date"))
                    try:
                        year = int(transaction_date[:4]) if transaction_date is not None else None
                    except ValueError:
                        year = None

                    transaction_sector_code = default_first(transaction.xpath("sector/@code"))
                    transaction_sector_vocabulary = default_first(transaction.xpath("sector/@vocabulary"))
                    if transaction_sector_code:
                        transaction_sector_percentage = 100
                    else:
                        transaction_sector_code = activity_sector_codes
                        transaction_sector_percentage = activity_sector_percentages
                        transaction_sector_vocabulary = activity_sector_vocabularies

                    transaction_country_code = default_first(transaction.xpath("recipient-country/@code"))
                    if transaction_country_code:
                        transaction_country_percentage = 100
                    else:
                        transaction_country_code = recipient_country_codes
                        transaction_country_percentage = recipient_country_percentages

                    transaction_description_narrative = default_first(transaction.xpath("description/narrative/text()"))

                    currency = default_first(transaction.xpath("value/@currency"))
                    currency = replace_default_if_none(currency, defaults["default-currency"])
                    if currency == "":
                        currency = None
                    if currency is not None:
                        currency = currency.replace(" ", "")

                    value = default_first(transaction.xpath("value/text()"))
                    try:
                        value = float(value.replace(" ", "")) if value is not None else None
                    except ValueError:
                        value = None
                    budget_type = None
                    budget_period_start = None
                    budget_period_end = None
                    b_or_t = "Transaction"

                    if value and currency:
                        if currency in self.dictionaries["ratedf"]:
                            converted_value = convert_usd(value, year, currency, self.dictionaries["ratedf"])
                        else:
                            pdb.set_trace()
                    else:
                        converted_value = "0"
                    row = [year, transaction_date, budget_period_start, budget_period_end, transaction_type_code, converted_value, b_or_t, budget_type, iati_identifier, reporting_org_name, reporting_org_ref, reporting_org_type, secondary_reporter, humanitarian, transaction_sector_code, transaction_sector_percentage, transaction_sector_vocabulary, transaction_country_code, transaction_country_percentage, finance_type_code, humanitarian_scope_narrative, humanitarian_scope_code, activity_title, activity_description, transaction_description_narrative, tag_code, tag_narrative, participating_org_name, participating_org_ref, participating_org_type, participating_org_role]
                    output.append(row)

            # Loop through budgets, and capture as close equivalents as we can to transactions
            if has_budget:
                budget_output = []
                budgets = activity.findall("budget")

                for budget in budgets:
                    transaction_type_code = None
                    if "type" in budget.attrib.keys():
                        budget_type = budget.attrib["type"]
                    else:
                        budget_type = None

                    transaction_date = default_first(budget.xpath("period-start/@iso-date"))
                    transaction_date_end = default_first(budget.xpath("period-end/@iso-date"))
                    time_range = {}
                    try:
                        time_range["start"] = dateutil.parser.parse(transaction_date)
                        time_range["end"] = dateutil.parser.parse(transaction_date_end)
                    except (TypeError, ValueError) as error:
                        time_range["start"] = None
                        time_range["end"] = None
                    if time_range["start"] is not None:
                        time_range["length"] = time_range["end"]-time_range["start"]
                        if time_range["length"] < datetime.timedelta(370):
                            year = time_range["start"].year

                            value = default_first(budget.xpath("value/text()"))
                            try:
                                value = float(value.replace(" ", "")) if value is not None else None
                            except ValueError:
                                value = None
                            currency = default_first(budget.xpath("value/@currency"))
                            currency = replace_default_if_none(currency, defaults["default-currency"])
                            if currency is not None:
                                currency = currency.replace(" ", "")

                            b_or_t = "Budget"

                            if value and currency:
                                if currency in self.dictionaries["ratedf"]:
                                    converted_value = convert_usd(value, year, currency, self.dictionaries["ratedf"])
                                else:
                                    pdb.set_trace()
                            else:
                                converted_value = "0"

                            row = [year, None, transaction_date, transaction_date_end, transaction_type_code, converted_value, b_or_t, budget_type, iati_identifier, reporting_org_name, reporting_org_ref, reporting_org_type, secondary_reporter, humanitarian, None, None, None, recipient_country_codes, recipient_country_percentages, defaults["default-finance-type"], humanitarian_scope_narrative, humanitarian_scope_code, activity_title, activity_description, None, tag_code, tag_narrative, participating_org_name, participating_org_ref, participating_org_type, participating_org_role]
                            meta = {"row": row, "time_range": time_range, "budget_type": budget_type}
                            budget_output.append(meta)
                if len(budget_output) > 1:
                    overlaps = []
                    spoiled = False
                    keep_indexes = list(range(0, len(budget_output)))
                    # All possible combinations of 2
                    for i in range(0, len(budget_output)):
                        if i+1 < len(budget_output):
                            for j in range(i+1, len(budget_output)):
                                first_budget = budget_output[i]
                                second_budget = budget_output[j]
                                if second_budget["time_range"]["end"] <= first_budget["time_range"]["end"] and second_budget["time_range"]["end"] >= first_budget["time_range"]["start"]:
                                    overlaps.append((i, j))
                                    if i in keep_indexes:
                                        keep_indexes.remove(i)
                                    if j in keep_indexes:
                                        keep_indexes.remove(j)
                                elif second_budget["time_range"]["start"] >= first_budget["time_range"]["start"] and second_budget["time_range"]["start"] <= first_budget["time_range"]["end"]:
                                    overlaps.append((i, j))
                                    if i in keep_indexes:
                                        keep_indexes.remove(i)
                                    if j in keep_indexes:
                                        keep_indexes.remove(j)
                    if len(overlaps) > 1:
                        for i, j in overlaps:
                            # If we've happened to put them back in the queue, take them out
                            if i in keep_indexes:
                                keep_indexes.remove(i)
                            if j in keep_indexes:
                                keep_indexes.remove(j)
                            budget1 = budget_output[i]
                            budget2 = budget_output[j]
                            # Only keep overlaps if one is revised and one is original
                            if budget1["budget_type"] == "1" and budget2["budget_type"] == "2":
                                keep_indexes.append(j)
                            elif budget1["budget_type"] == "2" and budget2["budget_type"] == "1":
                                keep_indexes.append(i)
                            elif budget1["budget_type"] == budget2["budget_type"]:
                                spoiled = True
                    if not spoiled:
                        for keep_index in keep_indexes:
                            output.append(budget_output[keep_index]["row"])
                elif len(budget_output) == 1:
                    # only one budget
                    output.append(budget_output[0]["row"])

        return output
