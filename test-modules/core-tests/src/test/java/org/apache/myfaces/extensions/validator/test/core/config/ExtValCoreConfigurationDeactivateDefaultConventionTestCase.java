package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationDeactivateDefaultConventionTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateDefaultConventionTestCase(
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
                    + ".DEACTIVATE_DEFAULT_CONVENTION", "true");
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
                public boolean deactivateDefaultConvention()
                {
                    return true;
                }

            }, true);
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "some better logic to see if it is really well integrated")
    public void testDeactivateDefConventionDefault() throws Exception
    {
        // Not a very clever testcase but code in
        // AbstractValidationErrorMessageResolver is too complex to have a quick
        // simple test of the parameter
        assertFalse(ExtValCoreConfiguration.get().deactivateDefaultConvention());
    }

    // Name of method should contain default
    public void testDeactivateDefConventionWebXml()
    {
        assertTrue(ExtValCoreConfiguration.get().deactivateDefaultConvention());
    }

    // Name of method should contain default
    public void testDeactivateDefConventionCustomConfig()
    {
        assertTrue(ExtValCoreConfiguration.get().deactivateDefaultConvention());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationDeactivateDefaultConventionTestCase.class);
    }

}
