package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.validation.parameter.DefaultValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationCustomValidationParameterExtractorFactoryClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomValidationParameterExtractorFactoryClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomValidationParameterExtractorFactory implements
            ValidationParameterExtractorFactory
    {

        public ValidationParameterExtractor create()
        {
            return null;
        }

    }

    public static class Custom2ValidationParameterExtractorFactory implements
            ValidationParameterExtractorFactory
    {

        public ValidationParameterExtractor create()
        {
            return null;
        }

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {

            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_VALIDATION_PARAMETER_EXTRACTOR_FACTORY",
                    CustomValidationParameterExtractorFactory.class.getName());
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
                public String customValidationParameterExtractorFactoryClassName()
                {
                    return Custom2ValidationParameterExtractorFactory.class
                            .getName();
                }

            }, true);
        }
    }

    public void testCustomValidationParameterExtractorFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.VALIDATION_PARAMETER_EXTRACTOR_FACTORY,
                Object.class);
        assertEquals(
                DefaultValidationParameterExtractorFactory.class.getName(),
                factory.getClass().getName());

    }

    public void testCustomValidationParameterExtractorFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.VALIDATION_PARAMETER_EXTRACTOR_FACTORY,
                Object.class);
        assertEquals(CustomValidationParameterExtractorFactory.class.getName(),
                factory.getClass().getName());
    }

    public void testCustomValidationParameterExtractorFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.VALIDATION_PARAMETER_EXTRACTOR_FACTORY,
                Object.class);
        assertEquals(
                Custom2ValidationParameterExtractorFactory.class.getName(),
                factory.getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomValidationParameterExtractorFactoryClassNameTestCase.class);
    }

}
