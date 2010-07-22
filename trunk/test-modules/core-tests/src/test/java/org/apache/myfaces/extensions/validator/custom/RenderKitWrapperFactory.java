package org.apache.myfaces.extensions.validator.custom;

import org.apache.myfaces.extensions.validator.core.renderkit.DefaultRenderKitWrapperFactory;

public class RenderKitWrapperFactory extends DefaultRenderKitWrapperFactory
{

	@Override
	protected boolean isApplicationInitialized()
	{
		// Required for testcase
		// ExtValCoreConfigurationDeactivateRenderKitFactoryTestCase
		// Otherwise VM parameter is checked for deactivation and not config parameter
		return true;
	}

}
