"""
RISK MANAGEMENT
Measuirng which supervised machine learning will be the best fit to determine whether a company pays back their loan, based on 
their financial statement. Meant to provide real-time insights that allows for quick robust decision making. 
Data: Company Financial Statement IDX 2020 - 2023
"""

import pandas as pd 
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import plotly.express as px
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, confusion_matrix, classification_report
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import plot_tree
import random
from sklearn.metrics import roc_curve, auc

# -----------------------------------------------------------------------------------------------------------------------------------------------

""" Random Colour Generator"""
def colour_generator(Number):
    x = []

    for i in range(Number):
        x.append(f"#{random.randint(0, 0xFFFFFF):06x}")
    
    return(x)

# -----------------------------------------------------------------------------------------------------------------------------------------------

"""Reproducible Logistic Regression"""
def logistic(X, y):
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # Scaled Data
    scaler = StandardScaler()
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)

    # Fit the model
    model = LogisticRegression(class_weight='balanced') # tells the model to focus on the minority class (0)
    model.fit(X_train, y_train)

    y_pred = model.predict(X_test)
    y_pred_log = model.predict_proba(X_test)[:, 1]

    # Coefficients
    coefficients = pd.DataFrame({
        'Feature': X.columns,
        'Coefficient': model.coef_[0] })
    pd.set_option('display.float_format', '{:.10f}'.format)

    return{
        "Accuracy": accuracy_score(y_test, y_pred), #Accuracy
        "Confusion Matrix": confusion_matrix(y_test, y_pred), #Confusion Materix
        "Classification Report": classification_report(y_test, y_pred), #Classification Report
        "Coefficients": coefficients,
        "Positive Class Log": y_pred_log
    }

# -----------------------------------------------------------------------------------------------------------------------------------------------
""" Coefficient Plot for Logistic Regression """

def log_coeff(data):
        fig_coeff = px.bar(
             data,
            x='Feature',
            y='Coefficient',
            color = colour_generator(len(data["Feature"])), # calls the colour generator 
            color_discrete_map="identity",
            title='Logistic Regression Coefficient',
            width=1200, height=1000 
        )

        fig_coeff.show()

# -----------------------------------------------------------------------------------------------------------------------------------------------
"""Random Forest"""

def Forest(X, y):

    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # Scaled Data
    scaler = StandardScaler()
    X_train = scaler.fit_transform(X_train)
    X_test = scaler.transform(X_test)

    # Checks for the best parameter
    param_grid = {
    'n_estimators': [100, 200],
    'max_depth': [2,3,5,10,20],
    'min_samples_split': [2, 5],
    'min_samples_leaf': [5,10],
    'bootstrap': [True, False]
    }

    grid_search = GridSearchCV(RandomForestClassifier(), param_grid=param_grid, cv=5)
    grid_search.fit(X_train, y_train)

    classifier = RandomForestClassifier(**grid_search.best_params_, oob_score = True, random_state = 42, class_weight = 'balanced')
    classifier.fit(X_train, y_train)
    y_pred = classifier.predict(X_test) 
    y_pred_rf = classifier.predict_proba(X_test)[:, 1]
    accuracy = accuracy_score(y_test, y_pred)

     ## 
    feature_imp = pd.DataFrame({
        'Feature': X.columns,
        'Importance': classifier.feature_importances_
    }).sort_values(by='Importance', ascending=False)

    return {
        "Accuracy": accuracy,
        "Classification Report": classification_report(y_test, y_pred),
        "Positive class RF": y_pred_rf,
        "Feature Imp": feature_imp,
        "Classifier": classifier
    }

# -----------------------------------------------------------------------------------------------------------------------------------------------
""" Importance of Variables in Random Forest """
def RF_importance(importance_df):
       fig = px.bar(
           importance_df,
            x='Importance',
            y='Feature',
            orientation='h',   # horizontal bar
            color = colour_generator(len(importance_df["Importance"])),
            color_discrete_map="identity",
            title='Random Forest Feature Importance'
           
       )

       fig.show()

# -----------------------------------------------------------------------------------------------------------------------------------------------
""" One tree from a random forest """
def one_tree(classifier, x):

    plt.figure(figsize=(5,10))
    plot_tree(
        classifier.estimators_[0],
        feature_names=X.columns,
        class_names=['0', '1'],
        filled=True,
        rounded=True
    )
    plt.title("One Decision Tree from the Random Forest")
    plt.show()


# -----------------------------------------------------------------------------------------------------------------------------------------------
""" ROC """
def ROC_comp(X, y, pos_log, pos_for):
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    test_df = pd.DataFrame(
    {'True': y_test, 'Logistic': pos_log, 'Random Forest': pos_for})

    plt.figure(figsize=(7, 5))

    for model in ['Logistic', 'Random Forest']:
        fpr, tpr, _ = roc_curve(test_df['True'], test_df[model])
        roc_auc = auc(fpr, tpr)
        plt.plot(fpr, tpr, label=f'{model} (AUC = {roc_auc:.2f})')

    plt.plot([0, 1], [0, 1], 'r--', label='Random Guess')

    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title('ROC Curves for Two Models')
    plt.legend()
    plt.show()





# -----------------------------------------------------------------------------------------------------------------------------------------------
""" Data Cleaning """
df = pd.read_csv("/Users/ardhyaandien/combined_financial_data_idx.csv")
df = df.drop(df.columns[[2,3,4,6]], axis=1) #drops columns 
df.head()

# replaces values to make it gramatically correct 
df['account'] = df['account'].replace('Cash Flowsfromusedin Operating Activities Direct', 'Cash Flow From Operating Activities')

# finds rows with any of these values
df = df[
    (df["account"] == "Accounts Receivable") |
    (df["account"] == "Cash And Cash Equivalents") |
    (df["account"] == "Cash Flow From Operating Activities") |
    (df["account"] == "Current Assets") |
    (df["account"] == "Current Liabilities") |
    (df["account"] == "Free Cash Flow") |
    (df["account"] == "Gross Profit") |
    (df["account"] == "Inventory") |
    (df["account"] == "Net Debt") |
    (df["account"] == "Net Income") |
    (df["account"] == "Repayment Of Debt") |
    (df["account"] == "Stockholders Equity") |
    (df["account"] == "Total Assets") |
    (df["account"] == "Total Liabilities Net Minority Interest") 
]

#show only 2022 values
# pivots df to have columns based on 'account'
df = df.pivot(index='symbol', columns='account', values='2022')
df = df.reset_index() #sets 'symbol' as new index
df.columns.name = None #prevents column names to be placed at "column level name"
df = df.rename(columns={'symbol': 'Company'}) # renames symbol as company
df.isna().sum() # measures how many NAs in a column
df = df.drop(columns=['Net Debt', 'Inventory', 'Accounts Receivable']) # drops sepcified columns (chosen with high NAs, or unecessary for prediction)
df = df.dropna() # removes rows with NAs

# Makes a new column, return on asset
df["ROA"] = df["Net Income"]/df["Total Assets"]

# Creates column Dept Equity
df["Dept Equity"] = df["Total Liabilities Net Minority Interest"]/df["Stockholders Equity"]

# Measures Current Ratio
df["Current Ratio"] = df["Current Assets"]/df["Current Liabilities"]

"""
changes value to binary. if there is repayment of debt (values are below 0, 
the higher the negative number is the higher the repayment of debt value is) then it is set to 1 = TRUE
Else, set to 0 = FALSE (not repayment)

"""
df['Repayment_Binary'] = np.where(df['Repayment Of Debt'] < 0, 1, 0)


sns.pairplot(df) # Plots a scatterplot to test correlated values 
plt.show()

# drops correlated values
df = df.drop(columns=['Gross Profit', 'Total Assets', 'Repayment Of Debt'])
#####
X = df.iloc[:, 1:11] # All columns except for the first column (company name) & last column (repayment binary)
y = df["Repayment_Binary"] # the value to be predicted



# -----------------------------------------------------------------------------------------------------------------------------------------------
""" Run the whole analysis """

# Logistic Regression 
result_log = logistic(X,y)
print(result_log)

# Plot 1
log_coeff(result_log["Coefficients"])

# Random Forest
result_for = Forest(X, y)
print(result_for)

# Plot 1
RF_importance(result_for["Feature Imp"])

# Plot 2
one_tree(result_for["Classifier"], X)

# ROC 
pos_log = result_log["Positive Class Log"]
pos_for = result_for["Positive class RF"]

ROC_comp(X, y, pos_log, pos_for)



