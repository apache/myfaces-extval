package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.DefaultMessageResolverFactory;
import org.apache.myfaces.extensions.validator.test.base.mock.MockMessageResolverFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

public class ExtValCoreConfigurationCustomMessageResolverFactoryClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomMessageResolverFactoryClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomMessageResolverFactory extends
            DefaultMessageResolverFactory
    {

    }

    public static class Custom2MessageResolverFactory extends
            DefaultMessageResolverFactory
    {

    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_MESSAGE_RESOLVER_FACTORY",
                    CustomMessageResolverFactory.class.getName());

        }
    }

    public void testCustomMessageResolverFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.MESSAGE_RESOLVER_FACTORY, Object.class);
        // The TestCase setup registers a Mockfactory so that a protected method
        // is made visible.
        // assertEquals(DefaultMessageResolverFactory.class.getName(), factory
        // .getClass().getName());
        assertEquals(MockMessageResolverFactory.class.getName(), factory
                .getClass().getName());

    }

    public void testCustomMessageResolverFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.MESSAGE_RESOLVER_FACTORY, Object.class);
        assertEquals(CustomMessageResolverFactory.class.getName(), factory
                .getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomMessageResolverFactoryClassNameTestCase.class);
    }

}
