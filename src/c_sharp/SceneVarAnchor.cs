using System;
using UnityEngine;
using clojure.lang;

public class SceneVarAnchor : UnityEngine.MonoBehaviour, UnityEngine.ISerializationCallbackReceiver
{
	[SerializeField]
	public string edn = "nil";

	private static bool _fullyInitialized = false;

	// ============================================================
	// clojure callouts

	private static IFn beforeSerializeFn;

	private static IFn afterDeserializeFn;

	private static IFn destroyFn;

	private static IFn requireFn;

	// ============================================================
	// the usual

	private static void require (string s)
	{
		if (requireFn == null) {
			requireFn = RT.var("clojure.core", "require");
		}
		requireFn.invoke(Symbol.intern(s));
	}

	private static void initializeVars ()
	{
		string nsStr = "timsg.scene-var";
		require(nsStr);
		if (beforeSerializeFn == null)
			beforeSerializeFn = RT.var(nsStr, "sva-serialize");
		if (afterDeserializeFn == null)
			afterDeserializeFn = RT.var(nsStr, "sva-deserialize");
		if (destroyFn == null)
			destroyFn = RT.var(nsStr, "sva-destroy");
	}

	// ============================================================
	// Serialization

	public void OnDestroy ()
	{
		Init();
		destroyFn.invoke(this);
	}


	public void OnBeforeSerialize ()
	{
		Init();
		edn = (string) beforeSerializeFn.invoke(this);
	}

	public void OnAfterDeserialize ()
	{
		Init();
		afterDeserializeFn.invoke(this);
	}

	public void Init ()
	{
		if (!_fullyInitialized) {
			initializeVars();
			_fullyInitialized = true;
		}
	}

}