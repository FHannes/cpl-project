Elm.Native.DateUtils = {};
Elm.Native.DateUtils.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.DateUtils = localRuntime.Native.DateUtils || {};
	if (localRuntime.Native.DateUtils.values)
	{
		return localRuntime.Native.DateUtils.values;
	}

	var now = Date.now();

	return localRuntime.Native.DateUtils.values = {
		now: now
	};

};
