import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

def calculate_chen_MODs(run):
    '''
    Given the name of an EI run on the 2012 Presidential election, this function aggregates
    the precinct-level EI-estimated votes up to the Texas House district level. It then uses
    the Article's definition of an minority opportunity district (MOD) to count the total
    number of MODs on the enacted Texas House district plan in 2012. We use this function to
    see how the MOD-count might change upon multiple reruns of the same EI setting.
    '''
    VTD_to_HD = pd.read_csv("../resources/TX_district_assignment.csv")

    df = pd.read_csv(f"../outputs/{run}.csv")
    df = pd.merge(df, VTD_to_HD, how='inner')
    df = df.groupby(by="sldl358").sum() # aggregate to the enacted TX HD plan

    BDvotes = "BDvotes"
    HDvotes = "HDvotes"
    BRvotes = "BRvotes"
    HRvotes = "HRvotes"
    ODvotes = "ODvotes"
    ORvotes = "ORvotes"

    obamaWins = (df[BDvotes] + df[HDvotes] + df[ODvotes]) > (df[BRvotes] + df[HRvotes] + df[ORvotes])
    romneyWins = ~obamaWins

    BHD_Outnumber_OD = (df[BDvotes] + df[HDvotes]) > df[ODvotes]
    BR_Outnumber_OR = df[BRvotes] > df[ORvotes]
    BD_Outnumber_OD = df[BDvotes] > df[ODvotes]
    HD_Outnumber_OD = df[HDvotes] > df[ODvotes]
    HR_Outnumber_OR = df[HRvotes] > df[ORvotes]
    BHR_Outnumber_OR = (df[BRvotes] + df[HRvotes]) > df[ORvotes]

    BHD = (df[BDvotes] > df[BRvotes]) & (df[HDvotes] > df[HRvotes])
    BR_HD = (df[BDvotes] <= df[BRvotes]) & (df[HDvotes] > df[HRvotes])
    BD_HR = (df[BDvotes] > df[BRvotes]) & (df[HDvotes] <= df[HRvotes])
    BHR = (df[BDvotes] <= df[BRvotes]) & (df[HDvotes] <= df[HRvotes])

    case1 = BHD & obamaWins & BHD_Outnumber_OD
    case2 = BR_HD & ((obamaWins & HD_Outnumber_OD) | (romneyWins & BR_Outnumber_OR))
    case3 = BD_HR & ((obamaWins & BD_Outnumber_OD) | (romneyWins & HR_Outnumber_OR))
    case4 = BHR & romneyWins & BHR_Outnumber_OR

    MODs = case1 | case2 | case3 | case4

#     print(f"Case 1: {sum(case1)}, {np.where(case1)}")
#     print(f"Case 2: {sum(case2)}, {np.where(case2)}")
#     print(f"Case 3: {sum(case3)}, {np.where(case3)}")
#     print(f"Case 4: {sum(case4)}, {np.where(case4)}")
#     print(f"None: {sum(~OODs)}, {np.where(~OODs)}")
    MOD_districts = list(np.where(MODs)[0]) # could return the actual list of districts, if wanted

    return sum(MODs)

def compare_runs(run1, run2):
    '''
    This function takes in the names of two EI runs. It will plot the
    share of the Black vote that President Obama won in the 2012 General
    Election, by precinct. The center plot will be a scatterplot with
    each run on its own axis. The marginal histograms plot each run on its own.
    '''
    df1 = pd.read_csv(f"../outputs/{run1}.csv")
    df2 = pd.read_csv(f"../outputs/{run2}.csv")

    fig = plt.figure(figsize=(8,8))
    gs = GridSpec(4,4)

    x = df1["BDvotes"] / (df1["BDvotes"] + df1["BRvotes"])
    y = df2["BDvotes"] / (df2["BDvotes"] + df2["BRvotes"])

    ax_joint = fig.add_subplot(gs[0:3,1:4])
    ax_marg_x = fig.add_subplot(gs[3,1:4])
    ax_marg_y = fig.add_subplot(gs[0:3,0])

    ax_joint.scatter(x,y)
    ax_joint.set_xlim(0,1)
    ax_joint.set_ylim(0,1)

    ax_marg_x.hist(x, bins=40, color="#8cb500")
    ax_marg_x.set_xlim(0,1)

    ax_marg_y.hist(y, bins=40, color="#d11a42", orientation="horizontal")
    ax_marg_y.set_ylim(0,1)
    ax_marg_y.set_xlim(ax_marg_x.get_ylim())
    ax_marg_y.invert_xaxis()

    plt.subplots_adjust(wspace=0.35, hspace=0.35)

    ax_joint.grid()
    ax_marg_x.grid(axis='x')
    ax_marg_y.grid(axis='y')

    for ax in [ax_marg_x, ax_marg_y]:
        ax.set_xticklabels([])
        ax.set_yticklabels([])
        ax.tick_params(left=False)
        ax.tick_params(right=False)
        ax.tick_params(top=False)
        ax.tick_params(bottom=False)

#     plt.savefig("../paper_plots/statewide_vs_county_one_phase_with_hists.png", dpi=300)
    plt.show()
    return
