package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.factory.DefaultFactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FacesMessageFactory;
import org.apache.myfaces.extensions.validator.core.factory.FactoryFinder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.validation.message.DefaultFacesMessageFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import javax.faces.application.FacesMessage;
import javax.faces.application.FacesMessage.Severity;

public class ExtValCoreConfigurationCustomFacesMessageFactoryClassNameTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationCustomFacesMessageFactoryClassNameTestCase(
            String name)
    {
        super(name);
    }

    public static class CustomFacesMessageFactory implements
            FacesMessageFactory
    {

        public FacesMessage convert(FacesMessage facesMessage)
        {
            return null;
        }

        public FacesMessage create(Severity severity, String summary,
                                   String detail)
        {
            return null;
        }

    }

    public static class Custom2FacesMessageFactory implements
            FacesMessageFactory
    {

        public FacesMessage convert(FacesMessage facesMessage)
        {
            return null;
        }

        public FacesMessage create(Severity severity, String summary,
                                   String detail)
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
                    + ".CUSTOM_FACES_MESSAGE_FACTORY",
                    CustomFacesMessageFactory.class.getName());

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
                public String customFacesMessageFactoryClassName()
                {
                    return Custom2FacesMessageFactory.class.getName();
                }

            }, true);
        }
    }

    public void testCustomFacesMessageFactoryClassNameDefault()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class);
        assertEquals(DefaultFacesMessageFactory.class.getName(), factory
                .getClass().getName());

    }

    public void testCustomFacesMessageFactoryClassNameWebXml()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class);
        assertEquals(CustomFacesMessageFactory.class.getName(), factory
                .getClass().getName());
    }

    public void testCustomFacesMessageFactoryClassNameCustomConfig()
    {
        FactoryFinder factoryFinder = DefaultFactoryFinder.getInstance();
        Object factory = factoryFinder.getFactory(
                FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class);
        assertEquals(Custom2FacesMessageFactory.class.getName(), factory
                .getClass().getName());
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationCustomFacesMessageFactoryClassNameTestCase.class);
    }

}
