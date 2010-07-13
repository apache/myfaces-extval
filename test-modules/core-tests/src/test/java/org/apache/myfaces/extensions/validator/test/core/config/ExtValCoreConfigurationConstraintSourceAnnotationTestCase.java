package org.apache.myfaces.extensions.validator.test.core.config;

import junit.framework.Test;
import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.base.util.ClassLoaderTestSuite;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAware2MetaDataBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareMetaDataBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.CustomConstraintSource;
import org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

public class ExtValCoreConfigurationConstraintSourceAnnotationTestCase extends
        ExtValCoreConfigurationTestCase
{
    public ExtValCoreConfigurationConstraintSourceAnnotationTestCase(String name)
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
                public Class<? extends Annotation> constraintSourceAnnotation()
                {
                    return CustomConstraintSource.class;
                }

            }, true);
        }
    }

    public void testConstraintSourceAnnotationDefault()
    {
        // Using the ConstraintSourceUtils.resolveMappedConstraintSourceFor
        // needs to much setup.

        Method method = ReflectionUtils.tryToGetMethod(
                ConstraintSourceUtils.class, "findMappedClass", Class.class,
                String.class);
        method.setAccessible(true);
        assertNotNull(method);
        Object[] args = new Object[2];
        args[0] = ConstraintSourceAwareBean.class;
        args[1] = "property2";
        Object result = ReflectionUtils.tryToInvokeMethodOfClass(
                ConstraintSourceUtils.class, method, args);
        assertNotNull(result);
        assertEquals(ConstraintSourceAwareMetaDataBean.class, result);

    }

    public void testConstraintSourceAnnotationWebXml()
    {
        // There is no Web.xml parameter defined yet;
    }

    public void testConstraintSourceAnnotationCustomConfig()
    {
        Method method = ReflectionUtils.tryToGetMethod(
                ConstraintSourceUtils.class, "findMappedClass", Class.class,
                String.class);
        method.setAccessible(true);
        assertNotNull(method);
        Object[] args = new Object[2];
        args[0] = ConstraintSourceAwareBean.class;
        args[1] = "property2";
        Object result = ReflectionUtils.tryToInvokeMethodOfClass(
                ConstraintSourceUtils.class, method, args);
        assertNotNull(result);
        assertEquals(ConstraintSourceAware2MetaDataBean.class, result);
    }

    public static Test suite()
    {

        return new ClassLoaderTestSuite(
                ExtValCoreConfigurationConstraintSourceAnnotationTestCase.class);
    }

}
