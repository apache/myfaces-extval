package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

public class ExtValCoreConfigurationActivateRequiredInitializationTestCase
        extends ExtValCoreConfigurationTestCase
{

    public ExtValCoreConfigurationActivateRequiredInitializationTestCase(
            String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".ACTIVATE_REQUIRED_INITIALIZATION", "true");
        }
    }

    @Override
    protected void invokeStartupListeners()
    {
        super.invokeStartupListeners();
        // This is the normal place where the user can intervene in the startup
        // and change the config
        if (needCustomConfig())
        {

            ExtValCoreConfiguration.use(new DefaultExtValCoreConfiguration()
            {

                @Override
                public boolean activateRequiredInitialization()
                {

                    return true;
                }

            }, true);
        }
    }

    public void testActivateRequiredInitializationDefault()
    {
        assertFalse(ExtValUtils.isRequiredInitializationActive());
    }

    public void testActivateRequiredInitializationWebXml()
    {
        assertTrue(ExtValUtils.isRequiredInitializationActive());
    }

    public void testActivateRequiredInitializationCustomConfig()
    {
        assertTrue(ExtValUtils.isRequiredInitializationActive());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationActivateRequiredInitializationTestCase.class);
    }
}
