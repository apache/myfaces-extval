package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.validation.strategy.DefaultValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.test.base.mock.MockValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationCustomValidationStrategyFactoryClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomValidationStrategyFactoryClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomValidationStrategyFactory extends
            DefaultValidationStrategyFactory
    {

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_VALIDATION_STRATEGY_FACTORY",
                    CustomValidationStrategyFactory.class.getName());

        }
    }

    public void testCustomValidationStrategyFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.VALIDATION_STRATEGY_FACTORY, Object.class);
        // The testcase install a MockFactory for it that exposes some protected
        // information
        // assertEquals(DefaultValidationStrategyFactory.class.getName(),
        // factory
        // .getClass().getName());
        assertEquals(MockValidationStrategyFactory.class.getName(), factory
                .getClass().getName());

    }

    public void testCustomValidationStrategyFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.VALIDATION_STRATEGY_FACTORY, Object.class);
        assertEquals(CustomValidationStrategyFactory.class.getName(), factory
                .getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomValidationStrategyFactoryClassNameTestCase.class);
    }

}
