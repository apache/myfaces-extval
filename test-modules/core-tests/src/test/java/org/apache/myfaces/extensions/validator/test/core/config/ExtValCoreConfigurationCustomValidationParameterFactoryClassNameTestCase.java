package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationCustomValidationParameterFactoryClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomValidationParameterFactoryClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomValidationParameterFactory extends
            DefaultValidationParameterFactory
    {

    }

    public static class Custom2ValidationParameterFactory extends
            DefaultValidationParameterFactory
    {

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            {

                addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                        + ".CUSTOM_VALIDATION_PARAMETER_FACTORY",
                        CustomValidationParameterFactory.class.getName());
            }
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
                public String customValidationParameterFactoryClassName()
                {
                    return Custom2ValidationParameterFactory.class.getName();
                }

            }, true);
        }
    }

    public void testCustomValidationParameterFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.VALIDATION_PARAMETER_FACTORY, Object.class);
        assertEquals(DefaultValidationParameterFactory.class.getName(), factory
                .getClass().getName());

    }

    public void testCustomValidationParameterFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.VALIDATION_PARAMETER_FACTORY, Object.class);
        assertEquals(CustomValidationParameterFactory.class.getName(), factory
                .getClass().getName());
    }

    public void testCustomValidationParameterFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.VALIDATION_PARAMETER_FACTORY, Object.class);
        assertEquals(Custom2ValidationParameterFactory.class.getName(), factory
                .getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomValidationParameterFactoryClassNameTestCase.class);
    }

}
