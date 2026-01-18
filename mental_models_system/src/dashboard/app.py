#!/usr/bin/env python3
"""
Mental Models Dashboard
Interactive dashboard for exploring the Mental Models System.

Built with Plotly Dash for rich visualizations and interactivity.
"""

import os
import sys
from typing import Dict, List, Optional
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import psycopg2
from psycopg2.extras import RealDictCursor

import dash
from dash import dcc, html, Input, Output, callback
import dash_bootstrap_components as dbc

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from config.settings import settings


# Initialize Dash app
app = dash.Dash(
    __name__,
    external_stylesheets=[dbc.themes.DARKLY],
    title="Mental Models System Dashboard"
)

# Database connection
def get_db_connection():
    """Get database connection."""
    return psycopg2.connect(
        dbname=settings.database.name,
        user=settings.database.user,
        host=settings.database.host,
        port=settings.database.port,
        password=settings.database.password or None,
        cursor_factory=RealDictCursor
    )


# Data loading functions
def load_system_stats() -> Dict:
    """Load system statistics."""
    conn = get_db_connection()
    cur = conn.cursor()
    
    stats = {}
    
    cur.execute("SELECT COUNT(*) as count FROM mental_models")
    stats['total_models'] = cur.fetchone()['count']
    
    cur.execute("SELECT COUNT(*) as count FROM thinker_principles")
    stats['total_principles'] = cur.fetchone()['count']
    
    cur.execute("SELECT COUNT(*) as count FROM case_studies")
    stats['total_cases'] = cur.fetchone()['count']
    
    cur.execute("SELECT COUNT(*) as count FROM frameworks")
    stats['total_frameworks'] = cur.fetchone()['count']
    
    cur.close()
    conn.close()
    
    return stats


def load_mental_models() -> pd.DataFrame:
    """Load mental models data."""
    conn = get_db_connection()
    query = """
        SELECT id, name, category, description, originator, 
               modern_synthesizer, lindy_age_years
        FROM mental_models
        ORDER BY category, name
    """
    df = pd.read_sql_query(query, conn)
    conn.close()
    return df


def load_case_studies() -> pd.DataFrame:
    """Load case studies data."""
    conn = get_db_connection()
    query = """
        SELECT id, name, date, category, region, severity, 
               financial_impact, models_involved, lollapalooza_score
        FROM case_studies
        ORDER BY date DESC
    """
    df = pd.read_sql_query(query, conn)
    conn.close()
    return df


def load_model_frequency() -> pd.DataFrame:
    """Load model frequency from Planck matrix."""
    conn = get_db_connection()
    query = """
        SELECT 
            model_name,
            COUNT(*) AS occurrence_count,
            ROUND(AVG(effect_size)::numeric, 3) AS avg_effect_size
        FROM planck_matrix
        GROUP BY model_name
        ORDER BY COUNT(*) DESC
        LIMIT 20
    """
    df = pd.read_sql_query(query, conn)
    conn.close()
    return df


def load_decade_analysis() -> pd.DataFrame:
    """Load decade analysis."""
    conn = get_db_connection()
    query = """
        SELECT 
            (EXTRACT(YEAR FROM date)::int / 10 * 10) AS decade,
            COUNT(*) AS case_count,
            ROUND(AVG(severity)::numeric, 2) AS avg_severity,
            ROUND(SUM(financial_impact)::numeric, 0) AS total_impact
        FROM case_studies
        WHERE date IS NOT NULL
        GROUP BY (EXTRACT(YEAR FROM date)::int / 10 * 10)
        ORDER BY decade
    """
    df = pd.read_sql_query(query, conn)
    conn.close()
    return df


# Layout
app.layout = dbc.Container([
    dbc.Row([
        dbc.Col([
            html.H1("🧠 Mental Models System", className="text-center mb-4 mt-4"),
            html.H4("The Oligarch's Operating System", className="text-center text-muted mb-4"),
        ])
    ]),
    
    # System Statistics
    dbc.Row([
        dbc.Col([
            dbc.Card([
                dbc.CardBody([
                    html.H4(id="stat-models", className="text-center"),
                    html.P("Mental Models", className="text-center text-muted")
                ])
            ], color="primary", outline=True)
        ], width=3),
        dbc.Col([
            dbc.Card([
                dbc.CardBody([
                    html.H4(id="stat-principles", className="text-center"),
                    html.P("Principles", className="text-center text-muted")
                ])
            ], color="success", outline=True)
        ], width=3),
        dbc.Col([
            dbc.Card([
                dbc.CardBody([
                    html.H4(id="stat-cases", className="text-center"),
                    html.P("Case Studies", className="text-center text-muted")
                ])
            ], color="info", outline=True)
        ], width=3),
        dbc.Col([
            dbc.Card([
                dbc.CardBody([
                    html.H4(id="stat-frameworks", className="text-center"),
                    html.P("Frameworks", className="text-center text-muted")
                ])
            ], color="warning", outline=True)
        ], width=3),
    ], className="mb-4"),
    
    # Tabs for different views
    dbc.Tabs([
        # Overview Tab
        dbc.Tab([
            dbc.Row([
                dbc.Col([
                    dcc.Graph(id="model-frequency-chart")
                ], width=6),
                dbc.Col([
                    dcc.Graph(id="category-distribution-chart")
                ], width=6),
            ], className="mt-4"),
            dbc.Row([
                dbc.Col([
                    dcc.Graph(id="decade-analysis-chart")
                ], width=12),
            ], className="mt-4"),
        ], label="Overview", tab_id="overview"),
        
        # Mental Models Tab
        dbc.Tab([
            dbc.Row([
                dbc.Col([
                    html.H5("Filter by Category", className="mt-4"),
                    dcc.Dropdown(
                        id="model-category-filter",
                        placeholder="Select category...",
                        className="mb-4"
                    ),
                ], width=4),
            ]),
            dbc.Row([
                dbc.Col([
                    html.Div(id="models-table")
                ], width=12),
            ]),
        ], label="Mental Models", tab_id="models"),
        
        # Case Studies Tab
        dbc.Tab([
            dbc.Row([
                dbc.Col([
                    html.H5("Filter Options", className="mt-4"),
                    dcc.Dropdown(
                        id="case-category-filter",
                        placeholder="Select category...",
                        className="mb-2"
                    ),
                    dcc.Dropdown(
                        id="case-region-filter",
                        placeholder="Select region...",
                        className="mb-4"
                    ),
                ], width=4),
            ]),
            dbc.Row([
                dbc.Col([
                    dcc.Graph(id="severity-distribution-chart")
                ], width=6),
                dbc.Col([
                    dcc.Graph(id="lollapalooza-chart")
                ], width=6),
            ], className="mt-4"),
            dbc.Row([
                dbc.Col([
                    html.Div(id="cases-table")
                ], width=12),
            ], className="mt-4"),
        ], label="Case Studies", tab_id="cases"),
        
        # Lollapalooza Analysis Tab
        dbc.Tab([
            dbc.Row([
                dbc.Col([
                    html.H5("Lollapalooza Effect Analysis", className="mt-4 mb-4"),
                    html.P("Cases where multiple mental models combine for multiplicative effects"),
                ], width=12),
            ]),
            dbc.Row([
                dbc.Col([
                    dcc.Graph(id="lollapalooza-scatter")
                ], width=12),
            ]),
            dbc.Row([
                dbc.Col([
                    dcc.Graph(id="models-interaction-heatmap")
                ], width=12),
            ], className="mt-4"),
        ], label="Lollapalooza", tab_id="lollapalooza"),
        
    ], id="tabs", active_tab="overview"),
    
    html.Footer([
        html.Hr(),
        html.P("Built for 100 years. Planck knowledge, not chauffeur knowledge.", 
               className="text-center text-muted")
    ], className="mt-5 mb-4")
    
], fluid=True)


# Callbacks
@callback(
    [Output("stat-models", "children"),
     Output("stat-principles", "children"),
     Output("stat-cases", "children"),
     Output("stat-frameworks", "children")],
    Input("tabs", "active_tab")
)
def update_stats(active_tab):
    """Update system statistics."""
    stats = load_system_stats()
    return (
        f"{stats['total_models']:,}",
        f"{stats['total_principles']:,}",
        f"{stats['total_cases']:,}",
        f"{stats['total_frameworks']:,}"
    )


@callback(
    Output("model-frequency-chart", "figure"),
    Input("tabs", "active_tab")
)
def update_model_frequency_chart(active_tab):
    """Update model frequency chart."""
    df = load_model_frequency()
    
    fig = px.bar(
        df,
        x="occurrence_count",
        y="model_name",
        orientation='h',
        title="Top 20 Most Frequently Applied Mental Models",
        labels={"occurrence_count": "Occurrences", "model_name": "Mental Model"},
        color="avg_effect_size",
        color_continuous_scale="Viridis"
    )
    
    fig.update_layout(
        template="plotly_dark",
        height=500,
        yaxis={'categoryorder': 'total ascending'}
    )
    
    return fig


@callback(
    Output("category-distribution-chart", "figure"),
    Input("tabs", "active_tab")
)
def update_category_distribution_chart(active_tab):
    """Update category distribution chart."""
    df = load_mental_models()
    category_counts = df['category'].value_counts()
    
    fig = px.pie(
        values=category_counts.values,
        names=category_counts.index,
        title="Mental Models by Category",
        hole=0.4
    )
    
    fig.update_layout(
        template="plotly_dark",
        height=500
    )
    
    return fig


@callback(
    Output("decade-analysis-chart", "figure"),
    Input("tabs", "active_tab")
)
def update_decade_analysis_chart(active_tab):
    """Update decade analysis chart."""
    df = load_decade_analysis()
    
    fig = make_subplots(
        rows=1, cols=2,
        subplot_titles=("Case Count by Decade", "Average Severity by Decade")
    )
    
    fig.add_trace(
        go.Bar(x=df['decade'], y=df['case_count'], name="Case Count"),
        row=1, col=1
    )
    
    fig.add_trace(
        go.Scatter(x=df['decade'], y=df['avg_severity'], mode='lines+markers', 
                   name="Avg Severity", line=dict(color='red')),
        row=1, col=2
    )
    
    fig.update_layout(
        template="plotly_dark",
        height=400,
        showlegend=False
    )
    
    return fig


@callback(
    Output("model-category-filter", "options"),
    Input("tabs", "active_tab")
)
def update_model_category_filter(active_tab):
    """Update model category filter options."""
    df = load_mental_models()
    categories = sorted(df['category'].dropna().unique())
    return [{"label": cat, "value": cat} for cat in categories]


@callback(
    Output("models-table", "children"),
    [Input("model-category-filter", "value")]
)
def update_models_table(selected_category):
    """Update models table based on filter."""
    df = load_mental_models()
    
    if selected_category:
        df = df[df['category'] == selected_category]
    
    # Create table
    table = dbc.Table.from_dataframe(
        df[['name', 'category', 'originator', 'lindy_age_years']].head(50),
        striped=True,
        bordered=True,
        hover=True,
        responsive=True,
        className="mt-4"
    )
    
    return table


@callback(
    [Output("case-category-filter", "options"),
     Output("case-region-filter", "options")],
    Input("tabs", "active_tab")
)
def update_case_filters(active_tab):
    """Update case study filter options."""
    df = load_case_studies()
    
    categories = sorted(df['category'].dropna().unique())
    regions = sorted(df['region'].dropna().unique())
    
    category_options = [{"label": cat, "value": cat} for cat in categories]
    region_options = [{"label": reg, "value": reg} for reg in regions]
    
    return category_options, region_options


@callback(
    Output("severity-distribution-chart", "figure"),
    [Input("case-category-filter", "value"),
     Input("case-region-filter", "value")]
)
def update_severity_distribution_chart(selected_category, selected_region):
    """Update severity distribution chart."""
    df = load_case_studies()
    
    if selected_category:
        df = df[df['category'] == selected_category]
    if selected_region:
        df = df[df['region'] == selected_region]
    
    fig = px.histogram(
        df,
        x="severity",
        nbins=30,
        title="Severity Distribution",
        labels={"severity": "Severity Score"}
    )
    
    fig.update_layout(
        template="plotly_dark",
        height=400
    )
    
    return fig


@callback(
    Output("lollapalooza-chart", "figure"),
    [Input("case-category-filter", "value"),
     Input("case-region-filter", "value")]
)
def update_lollapalooza_chart(selected_category, selected_region):
    """Update lollapalooza chart."""
    df = load_case_studies()
    
    if selected_category:
        df = df[df['category'] == selected_category]
    if selected_region:
        df = df[df['region'] == selected_region]
    
    fig = px.scatter(
        df,
        x="models_involved",
        y="lollapalooza_score",
        size="severity",
        color="category",
        hover_data=["name"],
        title="Lollapalooza Score vs Models Involved",
        labels={
            "models_involved": "Number of Models",
            "lollapalooza_score": "Lollapalooza Score"
        }
    )
    
    fig.update_layout(
        template="plotly_dark",
        height=400
    )
    
    return fig


@callback(
    Output("cases-table", "children"),
    [Input("case-category-filter", "value"),
     Input("case-region-filter", "value")]
)
def update_cases_table(selected_category, selected_region):
    """Update cases table based on filters."""
    df = load_case_studies()
    
    if selected_category:
        df = df[df['category'] == selected_category]
    if selected_region:
        df = df[df['region'] == selected_region]
    
    # Format financial impact
    df['financial_impact'] = df['financial_impact'].apply(
        lambda x: f"${x:,.0f}" if pd.notna(x) else "N/A"
    )
    
    # Create table
    table = dbc.Table.from_dataframe(
        df[['name', 'date', 'category', 'severity', 'models_involved', 'lollapalooza_score']].head(50),
        striped=True,
        bordered=True,
        hover=True,
        responsive=True,
        className="mt-4"
    )
    
    return table


@callback(
    Output("lollapalooza-scatter", "figure"),
    Input("tabs", "active_tab")
)
def update_lollapalooza_scatter(active_tab):
    """Update lollapalooza scatter plot."""
    df = load_case_studies()
    
    # Filter for high lollapalooza cases
    df_lolla = df[df['lollapalooza_score'] > 0.5]
    
    fig = px.scatter(
        df_lolla,
        x="models_involved",
        y="severity",
        size="lollapalooza_score",
        color="financial_impact",
        hover_data=["name", "category", "region"],
        title="High Lollapalooza Cases: Model Interaction vs Severity",
        labels={
            "models_involved": "Number of Models Involved",
            "severity": "Severity Score",
            "financial_impact": "Financial Impact"
        },
        color_continuous_scale="Reds"
    )
    
    fig.update_layout(
        template="plotly_dark",
        height=500
    )
    
    return fig


@callback(
    Output("models-interaction-heatmap", "figure"),
    Input("tabs", "active_tab")
)
def update_models_interaction_heatmap(active_tab):
    """Update models interaction heatmap."""
    # This would require a more complex query to get model co-occurrence
    # For now, create a placeholder
    
    conn = get_db_connection()
    query = """
        SELECT model_name, COUNT(*) as count
        FROM planck_matrix
        GROUP BY model_name
        ORDER BY count DESC
        LIMIT 10
    """
    df = pd.read_sql_query(query, conn)
    conn.close()
    
    # Create a simple heatmap of model frequencies
    fig = px.bar(
        df,
        x="model_name",
        y="count",
        title="Top 10 Models in High Lollapalooza Cases",
        labels={"count": "Occurrences", "model_name": "Mental Model"}
    )
    
    fig.update_layout(
        template="plotly_dark",
        height=400
    )
    
    return fig


if __name__ == "__main__":
    app.run_server(
        debug=True,
        host=settings.dashboard.host,
        port=settings.dashboard.port
    )
