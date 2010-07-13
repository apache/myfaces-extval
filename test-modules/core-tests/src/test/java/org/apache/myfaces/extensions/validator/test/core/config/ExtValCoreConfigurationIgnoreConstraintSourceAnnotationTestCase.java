package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.CustomIgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

public class ExtValCoreConfigurationIgnoreConstraintSourceAnnotationTestCase
        extends ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationIgnoreConstraintSourceAnnotationTestCase(
            String name)
    {
        super(name);
    }

    protected void addInitializationParameters()
    {
        super.addInitializationParameters();
        if (needXmlParameters())
        {
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
                public Class<? extends Annotation> ignoreConstraintSourceAnnotation()
                {
                    // TODO Auto-generated method stub
                    return CustomIgnoreConstraintSource.class;
                }

            }, true);
        }
    }

    public void testIgnoreConstraintSourceAnnotationDefault() throws Exception
    {
        Method method = ReflectionUtils.getMethod(ConstraintSourceUtils.class,
                "isMappedConstraintSourceIgnored", Class.class, String.class);
        assertNotNull(method);
        assertTrue((Boolean) ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property1"));
        assertFalse((Boolean) ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property2"));
    }

    public void testIgnoreConstraintSourceAnnotationCustomConfig()
            throws Exception
    {
        Method method = ReflectionUtils.getMethod(ConstraintSourceUtils.class,
                "isMappedConstraintSourceIgnored", Class.class, String.class);
        assertNotNull(method);
        assertFalse((Boolean) ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property1"));
        assertTrue((Boolean) ReflectionUtils.invokeMethodOfClass(
                ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property2"));
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationIgnoreConstraintSourceAnnotationTestCase.class);
    }

}
