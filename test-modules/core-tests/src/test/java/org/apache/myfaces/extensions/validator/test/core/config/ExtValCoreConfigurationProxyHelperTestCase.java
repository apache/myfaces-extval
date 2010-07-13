package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.proxy.ProxyHelper;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;

public class ExtValCoreConfigurationProxyHelperTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationProxyHelperTestCase(String name)
    {
        super(name);
    }

    public static class CustomProxyHelper implements ProxyHelper
    {

        public String getClassNameOfObject(Object proxiedObject)
        {
            return null;
        }

        public String getNameOfClass(Class proxiedClass)
        {
            return null;
        }

        public Class getUnproxiedClass(Class currentClass)
        {
            return null;
        }

        public <T> Class<T> getUnproxiedClass(Class currentClass,
                                              Class<T> targetType)
        {
            return null;
        }

        public boolean isProxiedClass(Class currentClass)
        {
            if (currentClass.equals(Object.class))
            {
                return true;
            }
            return false;
        }

        public boolean isProxiedObject(Object proxiedObject)
        {
            return false;
        }

    }

    @Override
    protected void setUp() throws Exception
    {
        super.addInitializationParameters();
        super.setUp();
        // Trick the method jsfUtils#isApplicationInitialized to believe the
        // application is initialized
        facesContext.getExternalContext().getRequestMap().put("Key", "Value");
    }

    protected void addInitializationParameters()
    {

        if (needXmlParameters())
        {
            addInitParameter(ExtValInformation.WEBXML_PARAM_PREFIX
                    + ".CUSTOM_PROXY_HELPER", CustomProxyHelper.class.getName());
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
                public ProxyHelper proxyHelper()
                {
                    return new CustomProxyHelper();
                }

            }, true);
        }
    }

    public void testProxyHelperDefault()
    {
        assertFalse(ProxyUtils.isProxiedClass(Object.class));
    }

    public void testProxyHelperWebXml()
    {
        assertTrue(ProxyUtils.isProxiedClass(Object.class));
    }

    public void testProxyHelperCustomConfig()
    {
        assertTrue(ProxyUtils.isProxiedClass(Object.class));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationProxyHelperTestCase.class);
    }

}
