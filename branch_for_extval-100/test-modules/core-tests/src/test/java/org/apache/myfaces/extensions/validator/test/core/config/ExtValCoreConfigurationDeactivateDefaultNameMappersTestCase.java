package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.factory.NameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.test.base.mock.MockMessageResolverFactory;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;

import java.util.List;

public class ExtValCoreConfigurationDeactivateDefaultNameMappersTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationDeactivateDefaultNameMappersTestCase(
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
                    + ".DEACTIVATE_DEFAULT_NAME_MAPPERS", "true");
        }
    }

    public void testDeactivateDefaultNameMappersDefault()
    {
        List<NameMapper<ValidationStrategy>> nameMappers = getNameMappers();
        assertFalse(nameMappers.isEmpty());
    }

    public void testDeactivateDefaultNameMappersWebXml()
    {
        List<NameMapper<ValidationStrategy>> nameMappers = getNameMappers();
        assertTrue(nameMappers.isEmpty());
    }

    private List<NameMapper<ValidationStrategy>> getNameMappers()
    {
        NameMapperAwareFactory result = ExtValContext.getContext()
                .getFactoryFinder().getFactory(
                        FactoryNames.MESSAGE_RESOLVER_FACTORY,
                        NameMapperAwareFactory.class);

        return ((MockMessageResolverFactory) result)
                .getRegisteredNameMapperList();
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationDeactivateDefaultNameMappersTestCase.class);
    }

}
