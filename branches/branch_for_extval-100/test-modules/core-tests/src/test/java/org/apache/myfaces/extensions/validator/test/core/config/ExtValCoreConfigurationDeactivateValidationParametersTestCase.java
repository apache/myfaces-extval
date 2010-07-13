package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

public class ExtValCoreConfigurationDeactivateValidationParametersTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateValidationParametersTestCase(
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
                    + ".DEACTIVATE_VALIDATION_PARAMETERS", "true");
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
                public boolean deactivateValidationParameters()
                {
                    return true;
                }

            }, true);
        }
    }

    public void testDeactivateValidationParametersDefault()
    {
        assertEquals(DefaultValidationParameterExtractor.class.getName(),
                ExtValUtils.getValidationParameterExtractor().getClass()
                        .getName());

    }

    public void testDeactivateValidationParametersWebXml()
    {
        assertTrue(ExtValUtils.getValidationParameterExtractor().getClass()
                .getName().startsWith(ExtValUtils.class.getName()));
    }

    public void testDeactivateValidationParametersCustomConfig()
    {
        assertTrue(ExtValUtils.getValidationParameterExtractor().getClass()
                .getName().startsWith(ExtValUtils.class.getName()));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationDeactivateValidationParametersTestCase.class);
    }

}
