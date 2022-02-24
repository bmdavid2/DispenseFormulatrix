
import argparse
import copy
import csv


def export_mantis_worklist(path, reagent_volumes):
    """
    Converts dictionary of reagent volumes into a worklist format readable by
    the Formulatrix Mantis.

    Inputs
    ------
    path: str
        Path to save the worklist, including its file name with extension. To be
        readable by the Mantis, it extension has to be '.dl.txt'.
    reagent_volumes: dict(str->pd.DataFrame)
        The keys of the dict are the reagent name to be used in the Mantis dispense.
        The values are a dataframe mapping out the rows and columns matching the layout
        of the dispense, where the values are the dispense volume in microliters.
    Outputs
    -------
    A dispense list saved at `path`.
    """

    num_reagents = len(reagent_volumes)
    delay_header = [num_reagents]
    for i in range(num_reagents):
        delay_header.extend([0, ""])

    with open(path, "w", newline="", encoding="utf-8") as f:
        # Header
        writer = csv.writer(f, delimiter="\t")
        writer.writerow(["[ Version: 5 ]"])
        writer.writerow([args.plate_type])
        writer.writerow(delay_header)
        writer.writerow([1])
        writer.writerow(delay_header)

        # Pipette volumes
        for reagent_name, vols in reagent_volumes.items():
            writer.writerow([reagent_name, "", "Normal"])
            writer.writerow(["Well", 1])
            writer.writerows(vols)

        print(f"Created worklist: {path}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="BacterAI Experiment Generator")

    parser.add_argument(
        "input_path",
        type=str,
        help="The input csv",
    )

    parser.add_argument(
        "output_name",
        type=str,
        help="The output worklist name",
    )

    parser.add_argument(
        "-p",
        "--plate_type",
        default="breakaway_pcr_96.pd.txt",
        type=str,
        help="The plate definition filename for Mantis.",
    )

    args = parser.parse_args()

    empty_plate = [[0 for _ in range(12)] for _ in range(8)]

    # input_path = "sample_input.csv"
    with open(args.input_path, 'r') as file:
        csv_file = list(csv.DictReader(file))

        ingredients = list(csv_file[0].keys())[: -2]
        volumes = {i: copy.deepcopy(empty_plate) for i in ingredients}
        for data in csv_file:
            row = int(data["Row"]) - 1
            col = int(data["Col"]) - 1
            for k, v in data.items():
                if k == "Col" or k == "Row":
                    continue
                volumes[k][row][col] = float(v)

        for k, vols in volumes.items():
            print()
            print("Reagent:", k)
            for row in vols:
                print(row)

    export_mantis_worklist(args.output_name + ".dl.txt", volumes)
