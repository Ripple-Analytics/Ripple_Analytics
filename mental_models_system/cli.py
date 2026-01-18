#!/usr/bin/env python3
"""
Mental Models System CLI

Command-line interface for analyzing documents through the lens of 129 mental models.

Usage:
    # Analyze a single file
    python cli.py analyze document.pdf
    
    # Analyze a directory
    python cli.py analyze-dir ./documents --recursive
    
    # Search the knowledge graph
    python cli.py search "network effects"
    
    # Find documents by model
    python cli.py find-by-model "Compounding"
    
    # Export knowledge graph
    python cli.py export --format json --output graph.json
    
    # Start API server
    python cli.py serve --port 8000
"""

import os
import sys
import json
import asyncio
import argparse
from pathlib import Path
from datetime import datetime
from typing import Optional

# Add src to path
sys.path.insert(0, str(Path(__file__).parent / "src"))


def setup_llm_client(args):
    """Set up the LLM client based on arguments."""
    from src.llm.local_llm import create_llm_client
    
    return create_llm_client(
        backend=args.backend,
        model=args.model,
        base_url=args.llm_url
    )


async def cmd_analyze(args):
    """Analyze a single document."""
    from src.analysis import MentalModelAnalyzer
    from src.pipeline.terabyte_processor import TextExtractor, SemanticChunker
    
    print(f"Analyzing: {args.file}")
    
    # Extract text
    text, metadata = TextExtractor.extract(args.file)
    if not text:
        print("Error: Could not extract text from file")
        return
    
    print(f"Extracted {len(text)} characters")
    
    # Set up LLM
    llm = setup_llm_client(args)
    analyzer = MentalModelAnalyzer(llm)
    
    # Chunk if needed
    if len(text) > 3000:
        chunker = SemanticChunker(chunk_size=2000, overlap=200)
        chunks = chunker.chunk(text)
        print(f"Split into {len(chunks)} chunks")
        analysis = await analyzer.analyze_chunks(chunks, Path(args.file).name)
    else:
        analysis = await analyzer.analyze_text(text, Path(args.file).name)
    
    # Output results
    print("\n" + "=" * 60)
    print("ANALYSIS RESULTS")
    print("=" * 60)
    
    print(f"\nDocument: {analysis.document_name}")
    print(f"Analyzed at: {analysis.analyzed_at}")
    print(f"Processing time: {analysis.processing_time:.2f}s")
    
    print(f"\n📊 Model Matches ({len(analysis.model_matches)} found):")
    for match in analysis.model_matches[:10]:
        print(f"  • {match.model_name} (relevance: {match.relevance_score:.2f})")
        if match.evidence:
            print(f"    Evidence: {match.evidence[:100]}...")
    
    if analysis.lollapalooza_alerts:
        print(f"\n🎯 Lollapalooza Effects ({len(analysis.lollapalooza_alerts)} found):")
        for alert in analysis.lollapalooza_alerts:
            print(f"  • Convergence score: {alert.convergence_score:.2f}")
            print(f"    Models: {', '.join(alert.models)}")
            print(f"    Description: {alert.description[:200]}...")
    
    print(f"\n📁 Categories: {', '.join(analysis.categories)}")
    print(f"🏷️  Tags: {', '.join(analysis.tags)}")
    
    if analysis.summary:
        print(f"\n📝 Summary:\n{analysis.summary}")
    
    # Save results
    if args.output:
        output_path = args.output
    else:
        output_path = f"{Path(args.file).stem}_analysis.json"
    
    with open(output_path, 'w') as f:
        json.dump(analysis.to_dict(), f, indent=2, default=str)
    
    print(f"\n✅ Results saved to: {output_path}")


async def cmd_analyze_dir(args):
    """Analyze all documents in a directory."""
    from src.analysis import BatchModelAnalyzer, build_graph_from_analyses
    
    print(f"Analyzing directory: {args.directory}")
    
    # Set up LLM
    llm = setup_llm_client(args)
    batch_analyzer = BatchModelAnalyzer(llm)
    
    # Set up progress callback
    def progress(current, total, analysis):
        print(f"[{current}/{total}] {analysis.document_name} - {len(analysis.model_matches)} models found")
    
    batch_analyzer.progress_callback = progress
    
    # Determine extensions
    extensions = args.extensions.split(',') if args.extensions else ['.txt', '.md', '.pdf']
    
    # Run analysis
    results = await batch_analyzer.analyze_directory(
        args.directory,
        extensions=extensions,
        max_files=args.max_files
    )
    
    # Print statistics
    stats = batch_analyzer.get_statistics()
    print("\n" + "=" * 60)
    print("BATCH ANALYSIS COMPLETE")
    print("=" * 60)
    print(f"Documents analyzed: {stats['total_documents']}")
    print(f"Unique models found: {stats['unique_models_found']}")
    print(f"Total model matches: {stats['total_model_matches']}")
    print(f"Lollapalooza alerts: {stats['total_lollapalooza_alerts']}")
    print(f"Avg models per document: {stats['avg_models_per_document']:.1f}")
    print(f"Avg processing time: {stats['avg_processing_time']:.1f}s")
    
    print("\nTop Models:")
    for model in stats['top_models'][:10]:
        print(f"  • {model['model']} ({model['count']} docs, avg relevance: {model['avg_relevance']:.2f})")
    
    # Build knowledge graph
    print("\nBuilding knowledge graph...")
    graph = build_graph_from_analyses(results)
    
    # Save results
    output_dir = args.output or "./analysis_results"
    os.makedirs(output_dir, exist_ok=True)
    
    # Export analyses
    batch_analyzer.export_results(f"{output_dir}/analyses.json")
    
    # Export graph
    graph.export_json(f"{output_dir}/knowledge_graph.json")
    graph.export_graphml(f"{output_dir}/knowledge_graph.graphml")
    
    print(f"\n✅ Results saved to: {output_dir}/")


async def cmd_search(args):
    """Search the knowledge graph."""
    from src.analysis import KnowledgeGraph
    
    # Load graph
    graph_path = args.graph or "./analysis_results/knowledge_graph.json"
    
    if not os.path.exists(graph_path):
        print(f"Error: Knowledge graph not found at {graph_path}")
        print("Run 'analyze-dir' first to build the graph")
        return
    
    graph = KnowledgeGraph()
    with open(graph_path, 'r') as f:
        data = json.load(f)
    
    for node_data in data.get("nodes", []):
        graph.add_node(
            type=node_data["type"],
            name=node_data["name"],
            properties=node_data.get("properties", {})
        )
    
    # Search
    results = graph.search(args.query, types=args.types.split(',') if args.types else None)
    
    print(f"\nSearch results for '{args.query}':")
    print("=" * 60)
    
    if not results:
        print("No results found")
        return
    
    for r in results[:20]:
        print(f"\n[{r['type'].upper()}] {r['name']}")
        if r.get('match_field') != 'name':
            print(f"  Matched in: {r.get('match_field')}")
        if r.get('properties'):
            for key, value in list(r['properties'].items())[:3]:
                if isinstance(value, str) and len(value) > 100:
                    value = value[:100] + "..."
                print(f"  {key}: {value}")


async def cmd_find_by_model(args):
    """Find documents by mental model."""
    from src.analysis import KnowledgeGraph
    
    # Load graph
    graph_path = args.graph or "./analysis_results/knowledge_graph.json"
    
    if not os.path.exists(graph_path):
        print(f"Error: Knowledge graph not found at {graph_path}")
        return
    
    graph = KnowledgeGraph()
    with open(graph_path, 'r') as f:
        data = json.load(f)
    
    for node_data in data.get("nodes", []):
        graph.add_node(
            type=node_data["type"],
            name=node_data["name"],
            properties=node_data.get("properties", {})
        )
    for edge_data in data.get("edges", []):
        graph.add_edge(
            source_id=edge_data["source"],
            target_id=edge_data["target"],
            type=edge_data["type"],
            weight=edge_data.get("weight", 1.0),
            properties=edge_data.get("properties", {})
        )
    
    # Find documents
    docs = graph.find_documents_by_model(args.model)
    
    print(f"\nDocuments applying '{args.model}':")
    print("=" * 60)
    
    if not docs:
        print("No documents found")
        return
    
    for d in docs:
        print(f"\n📄 {d['document']}")
        print(f"   Relevance: {d['relevance']:.2f}")
        if d.get('evidence'):
            print(f"   Evidence: {d['evidence'][:150]}...")


async def cmd_lollapalooza(args):
    """Find Lollapalooza effects across documents."""
    from src.analysis import KnowledgeGraph
    
    # Load graph
    graph_path = args.graph or "./analysis_results/knowledge_graph.json"
    
    if not os.path.exists(graph_path):
        print(f"Error: Knowledge graph not found at {graph_path}")
        return
    
    graph = KnowledgeGraph()
    with open(graph_path, 'r') as f:
        data = json.load(f)
    
    for node_data in data.get("nodes", []):
        graph.add_node(
            type=node_data["type"],
            name=node_data["name"],
            properties=node_data.get("properties", {})
        )
    for edge_data in data.get("edges", []):
        graph.add_edge(
            source_id=edge_data["source"],
            target_id=edge_data["target"],
            type=edge_data["type"],
            weight=edge_data.get("weight", 1.0),
            properties=edge_data.get("properties", {})
        )
    
    # Find Lollapalooza effects
    alerts = graph.find_lollapalooza_documents(min_score=args.min_score)
    
    print(f"\n🎯 Lollapalooza Effects (min score: {args.min_score}):")
    print("=" * 60)
    
    if not alerts:
        print("No Lollapalooza effects found")
        return
    
    for alert in alerts:
        print(f"\n📄 {alert['document']}")
        print(f"   Convergence Score: {alert['convergence_score']:.2f}")
        print(f"   Models: {', '.join(alert['models'])}")
        print(f"   Description: {alert['description'][:200]}...")
        if alert.get('implications'):
            print("   Implications:")
            for imp in alert['implications'][:3]:
                print(f"     • {imp}")


async def cmd_export(args):
    """Export knowledge graph."""
    from src.analysis import KnowledgeGraph
    
    # Load graph
    graph_path = args.graph or "./analysis_results/knowledge_graph.json"
    
    if not os.path.exists(graph_path):
        print(f"Error: Knowledge graph not found at {graph_path}")
        return
    
    graph = KnowledgeGraph()
    with open(graph_path, 'r') as f:
        data = json.load(f)
    
    for node_data in data.get("nodes", []):
        graph.add_node(
            type=node_data["type"],
            name=node_data["name"],
            properties=node_data.get("properties", {})
        )
    for edge_data in data.get("edges", []):
        graph.add_edge(
            source_id=edge_data["source"],
            target_id=edge_data["target"],
            type=edge_data["type"],
            weight=edge_data.get("weight", 1.0),
            properties=edge_data.get("properties", {})
        )
    
    # Export
    output = args.output or f"knowledge_graph.{args.format}"
    
    if args.format == "json":
        graph.export_json(output)
    elif args.format == "graphml":
        graph.export_graphml(output)
    elif args.format == "neo4j":
        graph.export_neo4j_cypher(output)
    else:
        print(f"Unknown format: {args.format}")
        return
    
    print(f"✅ Exported to: {output}")


async def cmd_serve(args):
    """Start the API server."""
    from src.api.server import create_app
    import uvicorn
    
    print(f"Starting Mental Models API server on port {args.port}...")
    
    app = create_app()
    
    config = uvicorn.Config(
        app,
        host=args.host,
        port=args.port,
        log_level="info"
    )
    server = uvicorn.Server(config)
    await server.serve()


async def cmd_failure_search(args):
    """Search failure modes."""
    from src.safeguards.failure_search import FailureModeSearchEngine
    
    engine = FailureModeSearchEngine()
    
    print(f"\nSearching failure modes for: '{args.query}'")
    print("=" * 60)
    
    results = engine.search(args.query, limit=args.limit)
    
    if not results:
        print("No failure modes found")
        return
    
    for r in results:
        print(f"\n[{r.relevance_score:.2f}] {r.model_name}: {r.mode_name}")
        print(f"   {r.description[:150]}...")
        if args.verbose:
            print(f"   Case: {r.real_world_case[:100]}...")
            print(f"   Warning signs: {', '.join(r.warning_signs[:3])}")
            print(f"   Safeguards: {', '.join(r.safeguards[:3])}")


async def cmd_assess_risk(args):
    """Assess risk for a decision or situation."""
    from src.safeguards.failure_search import FailureModeSearchEngine
    
    engine = FailureModeSearchEngine()
    
    # Parse models if provided
    models = args.models.split(',') if args.models else None
    
    print(f"\n🎯 Risk Assessment")
    print("=" * 60)
    print(f"Context: {args.context[:200]}..." if len(args.context) > 200 else f"Context: {args.context}")
    if models:
        print(f"Models: {', '.join(models)}")
    
    assessment = engine.assess_risk(args.context, models)
    
    print(f"\n📊 Overall Risk Score: {assessment.overall_risk_score:.2f}")
    
    # Risk level indicator
    if assessment.overall_risk_score >= 0.7:
        print("   ⚠️  HIGH RISK - Proceed with extreme caution")
    elif assessment.overall_risk_score >= 0.4:
        print("   ⚡ MODERATE RISK - Review safeguards")
    else:
        print("   ✅ LOW RISK - Standard precautions")
    
    print(f"\n📈 Risk by Category:")
    for cat, score in sorted(assessment.risk_by_category.items(), key=lambda x: x[1], reverse=True):
        bar = '█' * int(score * 10) + '░' * (10 - int(score * 10))
        print(f"   {cat}: {bar} {score:.2f}")
    
    print(f"\n⚠️  Top Warning Signs:")
    for sign in assessment.warning_signs_to_watch[:5]:
        print(f"   • {sign}")
    
    print(f"\n🛡️  Recommended Safeguards:")
    for safeguard in assessment.recommended_safeguards[:5]:
        print(f"   • {safeguard}")
    
    print(f"\n🔍 Top Failure Modes to Watch:")
    for fm in assessment.top_failure_modes[:5]:
        print(f"   [{fm.relevance_score:.2f}] {fm.model_name}: {fm.mode_name}")


async def cmd_failure_stats(args):
    """Show failure modes database statistics."""
    from src.safeguards.failure_search import FailureModeSearchEngine
    
    engine = FailureModeSearchEngine()
    stats = engine.get_statistics()
    
    print(f"\n📊 Failure Modes Database Statistics")
    print("=" * 60)
    print(f"Total models: {stats['total_models']}")
    print(f"Total failure modes: {stats['total_failure_modes']}")
    print(f"Average modes per model: {stats['average_modes_per_model']:.1f}")
    print(f"Unique words indexed: {stats['unique_words_indexed']}")
    
    print(f"\n📈 Modes by Category:")
    for cat, count in sorted(stats['modes_by_category'].items(), key=lambda x: x[1], reverse=True):
        bar = '█' * (count // 10) + '░' * max(0, 10 - count // 10)
        print(f"   {cat}: {bar} {count}")


async def cmd_improvements(args):
    """Generate and show improvement suggestions."""
    from src.improvement import ImprovementSuggestionEngine
    
    engine = ImprovementSuggestionEngine()
    
    if args.generate:
        print("\n🔄 Generating improvement suggestions...")
        suggestions = engine.analyze_and_suggest()
        print(f"Generated {len(suggestions)} new suggestions")
    
    print("\n📊 Improvement Suggestions")
    print("=" * 60)
    
    pending = engine.get_pending_suggestions()
    
    # Summary by priority
    from src.improvement import ImprovementPriority
    print(f"\nPending suggestions by priority:")
    for priority in ImprovementPriority:
        count = len([s for s in pending if s.priority == priority])
        bar = '█' * min(count, 20)
        print(f"  {priority.name:8}: {bar} {count}")
    
    # Show top suggestions
    if args.priority:
        filtered = [s for s in pending if s.priority.name.lower() == args.priority.lower()]
    else:
        filtered = engine.get_high_priority_suggestions()
    
    limit = args.limit or 10
    print(f"\n🎯 Top {min(limit, len(filtered))} Suggestions:")
    
    for i, s in enumerate(filtered[:limit], 1):
        print(f"\n{i}. [{s.priority.name}] {s.title}")
        print(f"   Impact: {s.estimated_impact:.0%} | Effort: {s.estimated_effort}")
        print(f"   {s.description[:150]}..." if len(s.description) > 150 else f"   {s.description}")
        if args.verbose:
            print(f"   Evidence: {', '.join(s.evidence[:2])}")
            print(f"   Steps: {s.implementation_steps[0]}...")


async def cmd_export_improvements(args):
    """Export improvement suggestions for Manus."""
    from src.improvement import ImprovementSuggestionEngine
    
    engine = ImprovementSuggestionEngine()
    
    output_path = args.output or None
    export_path = engine.export_for_manus(output_path)
    
    print(f"\n✅ Exported improvements to: {export_path}")
    print(f"\nThis file can be imported into Manus for implementation.")
    
    # Show summary
    pending = engine.get_pending_suggestions()
    print(f"\nExported {len(pending)} pending suggestions")


async def cmd_recommend(args):
    """Get decision recommendations."""
    from src.recommendation import get_recommendations, print_recommendations
    
    print(f"\n🎯 Analyzing decision: '{args.description}'")
    print(f"   Domain: {args.domain} | Stakes: {args.stakes}")
    print(f"   Time horizon: {args.time_horizon} | Reversibility: {args.reversibility}")
    
    recommendations = get_recommendations(
        description=args.description,
        domain=args.domain,
        stakes=args.stakes,
        time_horizon=args.time_horizon,
        reversibility=args.reversibility
    )
    
    if args.limit:
        recommendations = recommendations[:args.limit]
    
    print_recommendations(recommendations)
    
    if args.export:
        from src.recommendation import DecisionRecommender
        recommender = DecisionRecommender()
        export_path = recommender.export_recommendations(recommendations, args.export)
        print(f"\n✅ Exported to: {export_path}")


async def cmd_scheduler_list(args):
    """List all scheduled jobs."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    jobs = scheduler.list_jobs()
    
    print(f"\n📅 Scheduled Jobs ({len(jobs)} total)")
    print("=" * 70)
    
    if not jobs:
        print("No scheduled jobs")
        return
    
    for job in jobs:
        status = "✅" if job.enabled else "❌"
        print(f"\n{status} {job.name} ({job.id})")
        print(f"   Type: {job.schedule_type}")
        print(f"   Schedule: {job.schedule_value}")
        print(f"   Priority: {job.priority.name}")
        if job.next_run:
            print(f"   Next run: {job.next_run.strftime('%Y-%m-%d %H:%M:%S')}")
        if job.last_run:
            print(f"   Last run: {job.last_run.strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"   Runs: {job.run_count} (failures: {job.failure_count})")
        if args.verbose:
            print(f"   Handler: {job.handler}")
            print(f"   Description: {job.description}")


async def cmd_scheduler_add(args):
    """Add a new scheduled job."""
    from src.scheduler import JobScheduler, ScheduledJob, JobPriority
    
    scheduler = JobScheduler()
    
    priority = JobPriority[args.priority.upper()]
    
    job = ScheduledJob(
        id=args.id,
        name=args.name,
        description=args.description or f"Custom job: {args.name}",
        handler=args.handler,
        schedule_type=args.type,
        schedule_value=args.schedule,
        priority=priority,
        enabled=not args.disabled
    )
    
    if scheduler.add_job(job):
        print(f"✅ Added job: {args.name}")
        print(f"   ID: {args.id}")
        print(f"   Schedule: {args.type} - {args.schedule}")
        if job.next_run:
            print(f"   Next run: {job.next_run.strftime('%Y-%m-%d %H:%M:%S')}")
    else:
        print(f"❌ Failed to add job: {args.name}")


async def cmd_scheduler_remove(args):
    """Remove a scheduled job."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    
    if scheduler.remove_job(args.job_id):
        print(f"✅ Removed job: {args.job_id}")
    else:
        print(f"❌ Job not found: {args.job_id}")


async def cmd_scheduler_enable(args):
    """Enable or disable a scheduled job."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    
    if args.disable:
        if scheduler.disable_job(args.job_id):
            print(f"✅ Disabled job: {args.job_id}")
        else:
            print(f"❌ Job not found: {args.job_id}")
    else:
        if scheduler.enable_job(args.job_id):
            print(f"✅ Enabled job: {args.job_id}")
        else:
            print(f"❌ Job not found: {args.job_id}")


async def cmd_scheduler_run(args):
    """Run a scheduled job immediately."""
    from src.scheduler import JobScheduler
    import importlib
    
    scheduler = JobScheduler()
    
    if args.job_id not in scheduler.jobs:
        print(f"❌ Job not found: {args.job_id}")
        return
    
    job = scheduler.jobs[args.job_id]
    print(f"🔄 Running job: {job.name}")
    
    try:
        # Import and run the handler
        module_path, func_name = job.handler.rsplit('.', 1)
        module = importlib.import_module(module_path)
        handler = getattr(module, func_name)
        
        if asyncio.iscoroutinefunction(handler):
            result = await handler()
        else:
            result = handler()
        
        print(f"✅ Job completed successfully")
        if result:
            print(f"   Result: {result}")
    except Exception as e:
        print(f"❌ Job failed: {e}")


async def cmd_scheduler_stats(args):
    """Show scheduler statistics."""
    from src.scheduler import JobScheduler
    
    scheduler = JobScheduler()
    stats = scheduler.get_stats()
    
    print(f"\n📊 Scheduler Statistics")
    print("=" * 60)
    print(f"Total jobs: {stats['total_jobs']}")
    print(f"Enabled jobs: {stats['enabled_jobs']}")
    print(f"Total runs: {stats['total_runs']}")
    print(f"Total failures: {stats['total_failures']}")
    print(f"Webhooks configured: {stats.get('webhooks', 0)}")
    
    # Calculate success rate
    if stats['total_runs'] > 0:
        success_rate = ((stats['total_runs'] - stats['total_failures']) / stats['total_runs']) * 100
        print(f"Success rate: {success_rate:.1f}%")
    else:
        print(f"Success rate: N/A (no runs yet)")
    
    # Show jobs by type
    jobs = scheduler.list_jobs()
    by_type = {}
    by_priority = {}
    for job in jobs:
        by_type[job.schedule_type] = by_type.get(job.schedule_type, 0) + 1
        by_priority[job.priority.name] = by_priority.get(job.priority.name, 0) + 1
    
    print(f"\n📅 Jobs by Type:")
    for jtype, count in by_type.items():
        print(f"   {jtype}: {count}")
    
    print(f"\n⏰ Jobs by Priority:")
    for priority, count in by_priority.items():
        print(f"   {priority}: {count}")


async def cmd_webhook_list(args):
    """List all webhook targets."""
    from src.webhooks import WebhookManager
    
    manager = WebhookManager()
    targets = manager.list_targets()
    
    print(f"\n🔔 Webhook Targets ({len(targets)} total)")
    print("=" * 70)
    
    if not targets:
        print("No webhook targets configured")
        return
    
    for target in targets:
        status = "✅" if target.enabled else "❌"
        print(f"\n{status} {target.name} ({target.id})")
        print(f"   Type: {target.type.value}")
        print(f"   URL: {target.url[:50]}..." if len(target.url) > 50 else f"   URL: {target.url}")
        print(f"   Min Priority: {target.min_priority.name}")
        if target.events:
            print(f"   Events: {', '.join(e.value for e in target.events)}")
        else:
            print(f"   Events: All")
        if args.verbose:
            print(f"   Rate limit: {target.rate_limit_per_minute}/min")
            print(f"   Retry: {target.max_retries} retries")


async def cmd_webhook_add(args):
    """Add a new webhook target."""
    from src.webhooks import WebhookManager, WebhookTarget, WebhookType, EventType, EventPriority
    
    manager = WebhookManager()
    
    webhook_type = WebhookType(args.type)
    min_priority = EventPriority[args.priority.upper()]
    
    events = []
    if args.events:
        for event_name in args.events.split(','):
            events.append(EventType(event_name.strip()))
    
    target = WebhookTarget(
        id=args.id,
        name=args.name,
        type=webhook_type,
        url=args.url,
        events=events,
        min_priority=min_priority,
        enabled=not args.disabled
    )
    
    if manager.add_target(target):
        print(f"✅ Added webhook target: {args.name}")
        print(f"   ID: {args.id}")
        print(f"   Type: {args.type}")
        print(f"   URL: {args.url}")
    else:
        print(f"❌ Failed to add webhook target: {args.name}")


async def cmd_webhook_remove(args):
    """Remove a webhook target."""
    from src.webhooks import WebhookManager
    
    manager = WebhookManager()
    
    if manager.remove_target(args.target_id):
        print(f"✅ Removed webhook target: {args.target_id}")
    else:
        print(f"❌ Target not found: {args.target_id}")


async def cmd_webhook_test(args):
    """Send a test webhook."""
    from src.webhooks import WebhookManager, WebhookEvent, EventType, EventPriority
    
    manager = WebhookManager()
    
    event = WebhookEvent(
        type=EventType.SYSTEM_ALERT,
        priority=EventPriority.LOW,
        title="Test Webhook",
        message="This is a test message from the Mental Models System",
        data={"test": True, "timestamp": datetime.now().isoformat()}
    )
    
    print(f"🔔 Sending test webhook...")
    
    if args.target_id:
        # Send to specific target
        if args.target_id not in manager.targets:
            print(f"❌ Target not found: {args.target_id}")
            return
        
        target = manager.targets[args.target_id]
        success = await manager._send_to_target(target, event)
        if success:
            print(f"✅ Test webhook sent to: {target.name}")
        else:
            print(f"❌ Failed to send test webhook")
    else:
        # Send to all targets
        results = await manager.send(event)
        print(f"✅ Sent to {len(results)} targets")


async def cmd_webhook_stats(args):
    """Show webhook statistics."""
    from src.webhooks import WebhookManager
    
    manager = WebhookManager()
    stats = manager.get_stats()
    
    print(f"\n📊 Webhook Statistics")
    print("=" * 60)
    print(f"Total targets: {stats['total_targets']}")
    print(f"Enabled targets: {stats['enabled_targets']}")
    print(f"Total deliveries: {stats['total_deliveries']}")
    print(f"Successful: {stats['successful_deliveries']}")
    print(f"Failed: {stats['failed_deliveries']}")
    print(f"Success rate: {stats['success_rate']}")
    
    if stats['recent_deliveries']:
        print(f"\n📨 Recent Deliveries:")
        for delivery in stats['recent_deliveries'][:5]:
            status = "✅" if delivery['success'] else "❌"
            print(f"   {status} {delivery['target']} - {delivery['event_type']} ({delivery['timestamp']})")


async def cmd_data_export(args):
    """Export system data."""
    from src.data import DataExporter
    
    exporter = DataExporter()
    
    print(f"\n📤 Exporting data...")
    print(f"Format: {args.format}")
    print(f"Output: {args.output}")
    
    if args.format == 'json':
        path = exporter.export_to_json(args.output)
    elif args.format == 'sqlite':
        path = exporter.export_to_sqlite(args.output)
    elif args.format == 'csv':
        path = exporter.export_to_csv(args.output)
    elif args.format == 'parquet':
        path = exporter.export_to_parquet(args.output)
    else:
        print(f"❌ Unknown format: {args.format}")
        return
    
    print(f"✅ Exported to: {path}")
    
    # Show export stats
    stats = exporter.get_export_stats()
    print(f"\n📊 Export Statistics:")
    print(f"   Mental models: {stats.get('mental_models', 0)}")
    print(f"   Failure modes: {stats.get('failure_modes', 0)}")
    print(f"   Case studies: {stats.get('case_studies', 0)}")
    print(f"   Decisions: {stats.get('decisions', 0)}")


async def cmd_data_import(args):
    """Import system data."""
    from src.data import DataImporter
    
    importer = DataImporter()
    
    print(f"\n📥 Importing data...")
    print(f"Source: {args.source}")
    
    if args.merge:
        print("Mode: Merge (keep existing data)")
    else:
        print("Mode: Replace (overwrite existing data)")
    
    if args.source.endswith('.json'):
        result = importer.import_from_json(args.source, merge=args.merge)
    elif args.source.endswith('.db') or args.source.endswith('.sqlite'):
        result = importer.import_from_sqlite(args.source, merge=args.merge)
    elif args.source.endswith('.csv'):
        result = importer.import_from_csv(args.source, merge=args.merge)
    else:
        print(f"❌ Unknown file format")
        return
    
    print(f"✅ Import complete")
    print(f"\n📊 Import Results:")
    print(f"   Records imported: {result.get('imported', 0)}")
    print(f"   Records skipped: {result.get('skipped', 0)}")
    print(f"   Errors: {result.get('errors', 0)}")


async def cmd_audit_query(args):
    """Query audit logs."""
    from src.audit import audit_logger
    
    print(f"\n📋 Audit Log Query")
    print("=" * 60)
    
    events = audit_logger.query(
        event_type=args.type,
        actor=args.actor,
        resource_type=args.resource,
        limit=args.limit
    )
    
    if not events:
        print("No events found")
        return
    
    print(f"Found {len(events)} events:\n")
    
    for event in events:
        status = "✅" if event.outcome == "success" else "❌" if event.outcome == "failure" else "⏳"
        print(f"{status} [{event.timestamp.strftime('%Y-%m-%d %H:%M:%S')}] {event.event_type}")
        print(f"   Actor: {event.actor}")
        print(f"   Action: {event.action} on {event.resource_type}")
        if args.verbose and event.details:
            print(f"   Details: {event.details}")


async def cmd_audit_stats(args):
    """Show audit statistics."""
    from src.audit import audit_logger
    
    stats = audit_logger.get_stats()
    
    print(f"\n📊 Audit Statistics")
    print("=" * 60)
    print(f"Total events: {stats['total_events']}")
    print(f"Buffer size: {stats['buffer_size']}")
    print(f"Log file: {stats['log_file']}")
    print(f"Retention: {stats['retention_days']} days")
    
    print(f"\n📈 Events by Type:")
    for event_type, count in sorted(stats['events_by_type'].items(), key=lambda x: x[1], reverse=True)[:10]:
        bar = '█' * min(count, 20)
        print(f"   {event_type}: {bar} {count}")
    
    print(f"\n📊 Outcomes:")
    for outcome, count in stats['outcomes'].items():
        print(f"   {outcome}: {count}")


async def cmd_security_stats(args):
    """Show security statistics."""
    from src.api.security import RateLimiter, APIKeyManager
    
    rate_limiter = RateLimiter()
    key_manager = APIKeyManager()
    
    print(f"\n🔒 Security Statistics")
    print("=" * 60)
    
    # Rate limiting stats
    rl_stats = rate_limiter.get_stats()
    print(f"\n📊 Rate Limiting:")
    print(f"   Total requests: {rl_stats.get('total_requests', 0)}")
    print(f"   Blocked requests: {rl_stats.get('blocked_requests', 0)}")
    print(f"   Blocked IPs: {len(rl_stats.get('blocked_ips', []))}")
    
    # API key stats
    key_stats = key_manager.get_stats()
    print(f"\n🔑 API Keys:")
    print(f"   Total keys: {key_stats.get('total_keys', 0)}")
    print(f"   Active keys: {key_stats.get('active_keys', 0)}")
    print(f"   Expired keys: {key_stats.get('expired_keys', 0)}")


async def cmd_apikey_create(args):
    """Create a new API key."""
    from src.api.security import APIKeyManager
    
    manager = APIKeyManager()
    
    scopes = args.scopes.split(',') if args.scopes else ['read']
    
    key_info = manager.create_key(
        name=args.name,
        scopes=scopes,
        expires_days=args.expires
    )
    
    print(f"\n🔑 API Key Created")
    print("=" * 60)
    print(f"Name: {args.name}")
    print(f"Key: {key_info['key']}")
    print(f"Scopes: {', '.join(scopes)}")
    if args.expires:
        print(f"Expires: {key_info['expires_at']}")
    print(f"\n⚠️  Save this key - it cannot be retrieved later!")


async def cmd_apikey_list(args):
    """List all API keys."""
    from src.api.security import APIKeyManager
    
    manager = APIKeyManager()
    keys = manager.list_keys()
    
    print(f"\n🔑 API Keys ({len(keys)} total)")
    print("=" * 60)
    
    for key in keys:
        status = "✅" if key['active'] else "❌"
        print(f"\n{status} {key['name']}")
        print(f"   ID: {key['id']}")
        print(f"   Scopes: {', '.join(key['scopes'])}")
        print(f"   Created: {key['created_at']}")
        if key.get('expires_at'):
            print(f"   Expires: {key['expires_at']}")
        print(f"   Usage: {key.get('usage_count', 0)} requests")


async def cmd_apikey_revoke(args):
    """Revoke an API key."""
    from src.api.security import APIKeyManager
    
    manager = APIKeyManager()
    
    if manager.revoke_key(args.key_id):
        print(f"✅ Revoked API key: {args.key_id}")
    else:
        print(f"❌ Key not found: {args.key_id}")


async def cmd_similar_cases(args):
    """Find similar historical cases."""
    from src.safeguards.failure_search import FailureModeSearchEngine
    
    engine = FailureModeSearchEngine()
    
    print(f"\n🔍 Finding similar cases for: '{args.situation}'")
    print("=" * 60)
    
    cases = engine.find_similar_cases(args.situation, limit=args.limit)
    
    if not cases:
        print("No similar cases found")
        return
    
    for i, case in enumerate(cases, 1):
        print(f"\n{i}. [{case['relevance']:.2f}] {case['model']}: {case['failure_mode']}")
        print(f"   Case: {case['case'][:200]}..." if len(case['case']) > 200 else f"   Case: {case['case']}")
        if args.verbose:
            print(f"   Warning signs: {', '.join(case['warning_signs'][:3])}")
            print(f"   Safeguards: {', '.join(case['safeguards'][:3])}")


async def cmd_list_models(args):
    """List all available mental models."""
    from src.analysis import MentalModelLoader
    
    loader = MentalModelLoader()
    
    if args.category:
        models = loader.get_models_by_category(args.category)
        print(f"\nModels in category '{args.category}':")
    else:
        models = loader.get_all_models()
        print(f"\nAll {len(models)} Mental Models:")
    
    # Group by category
    by_category = {}
    for model in models:
        if model.category not in by_category:
            by_category[model.category] = []
        by_category[model.category].append(model)
    
    for category, cat_models in sorted(by_category.items()):
        print(f"\n{category} ({len(cat_models)} models):")
        for model in cat_models:
            print(f"  • {model.name}")
            if args.verbose:
                print(f"    {model.description[:100]}...")


def main():
    parser = argparse.ArgumentParser(
        description="Mental Models System CLI",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Analyze a document
  python cli.py analyze report.pdf --backend ollama --model llama3:70b
  
  # Analyze all PDFs in a directory
  python cli.py analyze-dir ./documents --extensions .pdf,.txt
  
  # Search the knowledge graph
  python cli.py search "network effects"
  
  # Find documents by mental model
  python cli.py find-by-model "Compounding"
  
  # Find Lollapalooza effects
  python cli.py lollapalooza --min-score 0.7
  
  # Export to Neo4j
  python cli.py export --format neo4j --output graph.cypher
  
  # Start API server
  python cli.py serve --port 8000
"""
    )
    
    # Global arguments
    parser.add_argument("--backend", default="ollama", 
                       choices=["ollama", "llamacpp", "vllm", "tgi", "openai"],
                       help="LLM backend to use")
    parser.add_argument("--model", default="llama3:70b", help="Model name")
    parser.add_argument("--llm-url", help="Custom LLM API URL")
    
    subparsers = parser.add_subparsers(dest="command", help="Command to run")
    
    # analyze command
    analyze_parser = subparsers.add_parser("analyze", help="Analyze a single document")
    analyze_parser.add_argument("file", help="Path to document")
    analyze_parser.add_argument("--output", "-o", help="Output file path")
    
    # analyze-dir command
    analyze_dir_parser = subparsers.add_parser("analyze-dir", help="Analyze all documents in a directory")
    analyze_dir_parser.add_argument("directory", help="Path to directory")
    analyze_dir_parser.add_argument("--extensions", default=".txt,.md,.pdf", help="File extensions to process")
    analyze_dir_parser.add_argument("--max-files", type=int, help="Maximum files to process")
    analyze_dir_parser.add_argument("--output", "-o", help="Output directory")
    analyze_dir_parser.add_argument("--recursive", "-r", action="store_true", help="Recursive search")
    
    # search command
    search_parser = subparsers.add_parser("search", help="Search the knowledge graph")
    search_parser.add_argument("query", help="Search query")
    search_parser.add_argument("--graph", help="Path to knowledge graph JSON")
    search_parser.add_argument("--types", help="Node types to search (comma-separated)")
    
    # find-by-model command
    find_parser = subparsers.add_parser("find-by-model", help="Find documents by mental model")
    find_parser.add_argument("model", help="Mental model name")
    find_parser.add_argument("--graph", help="Path to knowledge graph JSON")
    
    # lollapalooza command
    lolla_parser = subparsers.add_parser("lollapalooza", help="Find Lollapalooza effects")
    lolla_parser.add_argument("--min-score", type=float, default=0.7, help="Minimum convergence score")
    lolla_parser.add_argument("--graph", help="Path to knowledge graph JSON")
    
    # export command
    export_parser = subparsers.add_parser("export", help="Export knowledge graph")
    export_parser.add_argument("--format", choices=["json", "graphml", "neo4j"], default="json")
    export_parser.add_argument("--output", "-o", help="Output file path")
    export_parser.add_argument("--graph", help="Path to knowledge graph JSON")
    
    # serve command
    serve_parser = subparsers.add_parser("serve", help="Start API server")
    serve_parser.add_argument("--host", default="0.0.0.0", help="Host to bind")
    serve_parser.add_argument("--port", type=int, default=8000, help="Port to bind")
    
    # list-models command
    list_parser = subparsers.add_parser("list-models", help="List all mental models")
    list_parser.add_argument("--category", help="Filter by category")
    list_parser.add_argument("--verbose", "-v", action="store_true", help="Show descriptions")
    
    # failure-search command
    fm_search_parser = subparsers.add_parser("failure-search", help="Search failure modes")
    fm_search_parser.add_argument("query", help="Search query")
    fm_search_parser.add_argument("--limit", type=int, default=10, help="Maximum results")
    fm_search_parser.add_argument("--verbose", "-v", action="store_true", help="Show details")
    
    # assess-risk command
    risk_parser = subparsers.add_parser("assess-risk", help="Assess risk for a decision")
    risk_parser.add_argument("context", help="Description of the decision/situation")
    risk_parser.add_argument("--models", help="Mental models being applied (comma-separated)")
    
    # failure-stats command
    fm_stats_parser = subparsers.add_parser("failure-stats", help="Show failure modes statistics")
    
    # similar-cases command
    cases_parser = subparsers.add_parser("similar-cases", help="Find similar historical cases")
    cases_parser.add_argument("situation", help="Description of current situation")
    cases_parser.add_argument("--limit", type=int, default=5, help="Maximum cases")
    cases_parser.add_argument("--verbose", "-v", action="store_true", help="Show details")
    
    # improvements command
    improve_parser = subparsers.add_parser("improvements", help="Show improvement suggestions")
    improve_parser.add_argument("--generate", "-g", action="store_true", help="Generate new suggestions")
    improve_parser.add_argument("--priority", choices=["low", "medium", "high", "critical"], help="Filter by priority")
    improve_parser.add_argument("--limit", type=int, default=10, help="Maximum suggestions to show")
    improve_parser.add_argument("--verbose", "-v", action="store_true", help="Show details")
    
    # export-improvements command
    export_improve_parser = subparsers.add_parser("export-improvements", help="Export improvements for Manus")
    export_improve_parser.add_argument("--output", "-o", help="Output file path")
    
    # scheduler-list command
    sched_list_parser = subparsers.add_parser("scheduler-list", help="List all scheduled jobs")
    sched_list_parser.add_argument("--verbose", "-v", action="store_true", help="Show details")
    
    # scheduler-add command
    sched_add_parser = subparsers.add_parser("scheduler-add", help="Add a new scheduled job")
    sched_add_parser.add_argument("id", help="Unique job ID")
    sched_add_parser.add_argument("name", help="Job name")
    sched_add_parser.add_argument("handler", help="Handler function (e.g., src.module.function)")
    sched_add_parser.add_argument("--type", choices=["cron", "interval"], default="interval", help="Schedule type")
    sched_add_parser.add_argument("--schedule", required=True, help="Schedule value (cron expression or seconds)")
    sched_add_parser.add_argument("--priority", default="medium", choices=["low", "medium", "high", "critical"], help="Job priority")
    sched_add_parser.add_argument("--description", help="Job description")
    sched_add_parser.add_argument("--disabled", action="store_true", help="Create job disabled")
    
    # scheduler-remove command
    sched_remove_parser = subparsers.add_parser("scheduler-remove", help="Remove a scheduled job")
    sched_remove_parser.add_argument("job_id", help="Job ID to remove")
    
    # scheduler-enable command
    sched_enable_parser = subparsers.add_parser("scheduler-enable", help="Enable or disable a job")
    sched_enable_parser.add_argument("job_id", help="Job ID")
    sched_enable_parser.add_argument("--disable", action="store_true", help="Disable instead of enable")
    
    # scheduler-run command
    sched_run_parser = subparsers.add_parser("scheduler-run", help="Run a job immediately")
    sched_run_parser.add_argument("job_id", help="Job ID to run")
    
    # scheduler-stats command
    sched_stats_parser = subparsers.add_parser("scheduler-stats", help="Show scheduler statistics")
    
    # webhook-list command
    wh_list_parser = subparsers.add_parser("webhook-list", help="List all webhook targets")
    wh_list_parser.add_argument("--verbose", "-v", action="store_true", help="Show details")
    
    # webhook-add command
    wh_add_parser = subparsers.add_parser("webhook-add", help="Add a new webhook target")
    wh_add_parser.add_argument("id", help="Unique target ID")
    wh_add_parser.add_argument("name", help="Target name")
    wh_add_parser.add_argument("url", help="Webhook URL")
    wh_add_parser.add_argument("--type", default="generic", choices=["slack", "discord", "teams", "generic"], help="Webhook type")
    wh_add_parser.add_argument("--events", help="Event types to receive (comma-separated)")
    wh_add_parser.add_argument("--priority", default="low", choices=["low", "medium", "high", "critical"], help="Minimum priority")
    wh_add_parser.add_argument("--disabled", action="store_true", help="Create target disabled")
    
    # webhook-remove command
    wh_remove_parser = subparsers.add_parser("webhook-remove", help="Remove a webhook target")
    wh_remove_parser.add_argument("target_id", help="Target ID to remove")
    
    # webhook-test command
    wh_test_parser = subparsers.add_parser("webhook-test", help="Send a test webhook")
    wh_test_parser.add_argument("--target-id", help="Specific target to test (default: all)")
    
    # webhook-stats command
    wh_stats_parser = subparsers.add_parser("webhook-stats", help="Show webhook statistics")
    
    # data-export command
    data_export_parser = subparsers.add_parser("data-export", help="Export system data")
    data_export_parser.add_argument("--format", "-f", default="json",
                                    choices=["json", "sqlite", "csv", "parquet"],
                                    help="Export format")
    data_export_parser.add_argument("--output", "-o", required=True, help="Output path")
    
    # data-import command
    data_import_parser = subparsers.add_parser("data-import", help="Import system data")
    data_import_parser.add_argument("source", help="Source file path")
    data_import_parser.add_argument("--merge", "-m", action="store_true",
                                    help="Merge with existing data instead of replacing")
    
    # audit-query command
    audit_query_parser = subparsers.add_parser("audit-query", help="Query audit logs")
    audit_query_parser.add_argument("--type", "-t", help="Filter by event type")
    audit_query_parser.add_argument("--actor", "-a", help="Filter by actor")
    audit_query_parser.add_argument("--resource", "-r", help="Filter by resource type")
    audit_query_parser.add_argument("--limit", "-l", type=int, default=20, help="Maximum results")
    audit_query_parser.add_argument("--verbose", "-v", action="store_true", help="Show details")
    
    # audit-stats command
    audit_stats_parser = subparsers.add_parser("audit-stats", help="Show audit statistics")
    
    # security-stats command
    security_stats_parser = subparsers.add_parser("security-stats", help="Show security statistics")
    
    # apikey-create command
    apikey_create_parser = subparsers.add_parser("apikey-create", help="Create a new API key")
    apikey_create_parser.add_argument("name", help="Key name")
    apikey_create_parser.add_argument("--scopes", "-s", default="read",
                                      help="Comma-separated scopes (read,write,admin)")
    apikey_create_parser.add_argument("--expires", "-e", type=int,
                                      help="Days until expiration")
    
    # apikey-list command
    apikey_list_parser = subparsers.add_parser("apikey-list", help="List all API keys")
    
    # apikey-revoke command
    apikey_revoke_parser = subparsers.add_parser("apikey-revoke", help="Revoke an API key")
    apikey_revoke_parser.add_argument("key_id", help="Key ID to revoke")
    
    # recommend command
    recommend_parser = subparsers.add_parser("recommend", help="Get decision recommendations")
    recommend_parser.add_argument("description", help="Description of the decision")
    recommend_parser.add_argument("--domain", "-d", default="business",
                                  choices=["investment", "business", "personal", "career"],
                                  help="Decision domain")
    recommend_parser.add_argument("--stakes", "-s", default="medium",
                                  choices=["low", "medium", "high", "critical"],
                                  help="Stakes level")
    recommend_parser.add_argument("--time-horizon", "-t", default="medium",
                                  choices=["short", "medium", "long"],
                                  help="Time horizon")
    recommend_parser.add_argument("--reversibility", "-r", default="partially_reversible",
                                  choices=["reversible", "partially_reversible", "irreversible"],
                                  help="Decision reversibility")
    recommend_parser.add_argument("--limit", "-l", type=int, help="Maximum recommendations")
    recommend_parser.add_argument("--export", "-e", help="Export to file")
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    # Run command
    if args.command == "analyze":
        asyncio.run(cmd_analyze(args))
    elif args.command == "analyze-dir":
        asyncio.run(cmd_analyze_dir(args))
    elif args.command == "search":
        asyncio.run(cmd_search(args))
    elif args.command == "find-by-model":
        asyncio.run(cmd_find_by_model(args))
    elif args.command == "lollapalooza":
        asyncio.run(cmd_lollapalooza(args))
    elif args.command == "export":
        asyncio.run(cmd_export(args))
    elif args.command == "serve":
        asyncio.run(cmd_serve(args))
    elif args.command == "list-models":
        asyncio.run(cmd_list_models(args))
    elif args.command == "failure-search":
        asyncio.run(cmd_failure_search(args))
    elif args.command == "assess-risk":
        asyncio.run(cmd_assess_risk(args))
    elif args.command == "failure-stats":
        asyncio.run(cmd_failure_stats(args))
    elif args.command == "similar-cases":
        asyncio.run(cmd_similar_cases(args))
    elif args.command == "improvements":
        asyncio.run(cmd_improvements(args))
    elif args.command == "export-improvements":
        asyncio.run(cmd_export_improvements(args))
    elif args.command == "recommend":
        asyncio.run(cmd_recommend(args))
    elif args.command == "scheduler-list":
        asyncio.run(cmd_scheduler_list(args))
    elif args.command == "scheduler-add":
        asyncio.run(cmd_scheduler_add(args))
    elif args.command == "scheduler-remove":
        asyncio.run(cmd_scheduler_remove(args))
    elif args.command == "scheduler-enable":
        asyncio.run(cmd_scheduler_enable(args))
    elif args.command == "scheduler-run":
        asyncio.run(cmd_scheduler_run(args))
    elif args.command == "scheduler-stats":
        asyncio.run(cmd_scheduler_stats(args))
    elif args.command == "webhook-list":
        asyncio.run(cmd_webhook_list(args))
    elif args.command == "webhook-add":
        asyncio.run(cmd_webhook_add(args))
    elif args.command == "webhook-remove":
        asyncio.run(cmd_webhook_remove(args))
    elif args.command == "webhook-test":
        asyncio.run(cmd_webhook_test(args))
    elif args.command == "webhook-stats":
        asyncio.run(cmd_webhook_stats(args))
    elif args.command == "data-export":
        asyncio.run(cmd_data_export(args))
    elif args.command == "data-import":
        asyncio.run(cmd_data_import(args))
    elif args.command == "audit-query":
        asyncio.run(cmd_audit_query(args))
    elif args.command == "audit-stats":
        asyncio.run(cmd_audit_stats(args))
    elif args.command == "security-stats":
        asyncio.run(cmd_security_stats(args))
    elif args.command == "apikey-create":
        asyncio.run(cmd_apikey_create(args))
    elif args.command == "apikey-list":
        asyncio.run(cmd_apikey_list(args))
    elif args.command == "apikey-revoke":
        asyncio.run(cmd_apikey_revoke(args))


if __name__ == "__main__":
    main()
